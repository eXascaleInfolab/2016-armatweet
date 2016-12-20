package xi.armatweet.nlp

import cmu.arktweetnlp.{Tagger, Twokenize}
import cmu.arktweetnlp.impl.{ModelSentence, Sentence}
import xi.armatweet.nlp.Tweet.{Token, _}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

/**
  * Created by alberto on 10/08/16.
  */
class Tweet(val originalText: String, tokens: Seq[Token]) {

  private val stringRepr = tokens.map { case Token(str, _, _, ttype) => s"$str/$ttype" }.mkString(" ")

  val text = tokens.map(_.token).mkString(" ")

  override def toString: String = stringRepr


  /**
    * Given an offset in the textual representation of the tweet
    * (the string obtained by calling the toString method), gives
    * back the tokens related to the substring identified by the
    * input offset.
    *
    * @param startOffset (inclusive)
    * @param endOffset   (exclusive)
    * @return a sequence of triples (token_idx, startOffset, beginOffset)
    *         where token_idx is the index of the toke in `tokens`, and the
    *         offset refers to its string representation.
    */
  def stringOffsetToTokens(startOffset: Int, endOffset: Int) = {
    var tokenBeginningInStr = 0
    val acc = ArrayBuffer[(Int, Int, Int)]()
    for ((token, tokenIdx) <- tokens.zipWithIndex) {
      val startsAfterCurrToken = startOffset >= tokenBeginningInStr + token.token.length
      val finishesBeforeCurrentToken = endOffset <= tokenBeginningInStr

      if (!startsAfterCurrToken && !finishesBeforeCurrentToken) {
        //current token must be included in the result

        //how much of the current token shall we include?
        val tokenOffsetS = if (startOffset < tokenBeginningInStr)
          0 // it was already started few tokens ago
        else
          startOffset - tokenBeginningInStr //it starts in the current token

        val tokenOffsetE = if (endOffset > tokenBeginningInStr + token.token.length)
          token.token.length // it ends on some future token
        else
          endOffset - tokenBeginningInStr // it ends in this token
        acc += ((tokenIdx, tokenOffsetS, tokenOffsetE))
      } //if
      tokenBeginningInStr += token.token.length + 1 // also consider the space added to make stringRepr
    } //for
    acc.result()
  } //stringOffsetToTokens

  /**
    * Given an offset in the string representation of the current object,
    * returns its offset in the original text this object derives from.
    * It works token-wise: the returned string is rounded up to fill
    * the last token of the object.
    *
    * @param startOffset
    * @param endOffset
    * @return
    */
  def toOriginalStringOffset_roundedTokens(startOffset: Int, endOffset: Int) = {
    val tokensIdx = stringOffsetToTokens(startOffset, endOffset)
    if (tokensIdx.nonEmpty) {
      val headIdx = tokensIdx.head._1
      val lastIdx = tokensIdx.last._1

      val startSpan = tokens(headIdx).start
      val endSpan = tokens(lastIdx).end
      Some(startSpan, endSpan)
    } else {
      None
    } //if
  } //toOriginalStringOffset


  def toOriginalStringOffset(startOffset: Int, endOffset: Int) = {
    /* a sequence of triples (token_idx, startOffset, beginOffset)
     * where token_idx is the index of the toke in `tokens`, and the
     * offset refers to its string representation. */
    val tokensIdx = stringOffsetToTokens(startOffset, endOffset)
    if (tokensIdx.nonEmpty) {
      val (startIdx, startB, startE) = tokensIdx.head
      val (endIdx, endB, endE) = tokensIdx.last

      val startSpan = tokens(startIdx).start + startB
      val endSpan = tokens(endIdx).end - (tokens(endIdx).token.length - endE)
      Some(startSpan, endSpan)
    } else {
      None
    } //if
  } //toOriginalStringOffset


  def splitCamelCase() = {
    val acc = new mutable.ArrayBuffer[Token]()
    for (Token(token, s, e, tokenType) <- tokens) {
      val delimiters = TextUtils.CamelCaseRE.findAllIn(token).matchData
      if (delimiters.isEmpty || tokenType == Url) {
        //don't touch URLs
        acc += Token(token, s, e, tokenType)
      } else {
        var wordStart = 0
        for (delim <- delimiters) {
          val wordEnd = delim.start
          if (wordStart < wordEnd) {
            acc += Token(token.substring(wordStart, wordEnd),
              s + wordStart,
              s + wordEnd,
              tokenType)
          }
          wordStart = delim.end
        } //for

        if (wordStart < token.length) {
          acc += Token(token.substring(wordStart), s + wordStart, e, tokenType)
        }
      } //if-delimiter
    } //for
    new Tweet(originalText, acc.result())
  } //splitCamelCase


  def removeEndingTagList(): Tweet = {
    //     first find the ending tag list in the original text and then remove it from `tokens`
    val toRemove = TextUtils.EndingTagListRE.findAllIn(originalText).matchData.toList.headOption
    toRemove match {
      case None => new Tweet(originalText, tokens)
      case Some(md) =>
        val takeUntilIndex = md.start
        val goodTokens = tokens.takeWhile { tok => tok.start <= takeUntilIndex && tok.end <= takeUntilIndex }
        new Tweet(originalText, goodTokens)
    }
  } //removeEndingTagList

  def removeHashHandlesChars() = {
    val newTokens = tokens.map {
      case Token(str, b, e, HashTag) => Token(str.substring(1), b + 1, e, HashTag)
      case Token(str, b, e, Handle) => Token(str.substring(1), b + 1, e, Handle)
      case t => t
    }
    new Tweet(originalText, newTokens)
  } //removeHashHandlesChars


  /**
    * Trims the tweet from starting and finishing discourse markers (like RT)
    */
  def trimDiscourseMarkers() = {
    val newTokens = tokens.dropWhile(_.tokType == DiscourseMarker)
      .reverse
      .dropWhile(_.tokType == DiscourseMarker)
      .reverse
    new Tweet(originalText, newTokens)
  } //trimDiscourseMarkers


  def removeURLs() = {
    val goodTokens = tokens.filterNot(_.tokType == Url)
    new Tweet(originalText, goodTokens)
  }


  def clean() = this
    .trimDiscourseMarkers()
    .removeEndingTagList()
    .removeHashHandlesChars()
    .removeURLs()
    .splitCamelCase()
    .cleanWeirdTokens


//    val GoodSymbols_re = "[\\w\\p{Punct}]+".r
  val GoodSymbols_re = "[\\w\\!\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^_\\`\\{\\|\\}\\~\\’\u201C\u201D\u2018\u00B4\u0060]+".r
  private def removeWeirdCharsFromToken(tok: Token) = {
    tok.token match {
      case GoodSymbols_re() => Seq(tok) // the token is made by good symbols
      case tok_str =>
        GoodSymbols_re.findAllMatchIn(tok_str).map { m =>
          // split the token into sub-tokens of good characters
          Token(tok_str.substring(m.start, m.end), tok.start + m.start, tok.start + m.end, tok.tokType)
        }
    } //match
  } //removeWeirdCharsFromToken

  def cleanWeirdTokens = {
    new Tweet(originalText, tokens.flatMap(removeWeirdCharsFromToken))
  } //cleanWeirdTokens


  def serializeStr = {
    tokens.map(t => s"${t.token}\t${t.start}\t${t.end}\t${t.tokType}").
      mkString("\n")
  }

}

//Tweet


object Tweet {

  /**
    * From Table 6 of "Improved Part-Of-Speach Tagging for ONline Conversational Text with Word Clusters
    * by Owoputi et al.
    */
  sealed trait TokenType

  case object HashTag extends TokenType

  case object Handle extends TokenType

  case object Url extends TokenType

  case object CommonNoun extends TokenType

  case object Pronoun extends TokenType

  case object PorperNoun extends TokenType

  case object NominalPossessive extends TokenType

  case object ProperPossessive extends TokenType

  case object Verb extends TokenType

  case object NominalVerbal extends TokenType

  case object ProperNounVerbal extends TokenType

  case object Adjective extends TokenType

  case object Adverb extends TokenType

  case object Interjection extends TokenType

  case object Determiner extends TokenType

  case object PrePostPosition extends TokenType

  case object Conjunction extends TokenType

  case object VerbParticle extends TokenType

  case object Existential extends TokenType

  case object ExistentialVerbal extends TokenType

  case object DiscourseMarker extends TokenType

  case object Emoticon extends TokenType

  case object Numeral extends TokenType

  case object Punctuation extends TokenType

  case object Garbage extends TokenType

  case class Token(token: String, start: Int, end: Int, tokType: TokenType)

  def translateTokenType(token: String, typeStr: String): TokenType = typeStr match {
    case x if token.startsWith("#") && token.length > 1 => HashTag // hashtags composing proper nouns are not marked as # but as ^
    case x if token.startsWith("@") && token.length > 1 => Handle // hashtags composing proper nouns are not marked as # but as ^
    case "N" => CommonNoun
    case "O" => Pronoun
    case "^" => PorperNoun
    case "S" => NominalPossessive
    case "Z" => ProperPossessive
    case "V" => Verb
    case "L" => NominalVerbal
    case "M" => ProperNounVerbal
    case "A" => Adjective
    case "R" => Adverb
    case "!" => Interjection
    case "D" => Determiner
    case "P" => PrePostPosition
    case "&" => Conjunction
    case "T" => VerbParticle
    case "X" => Existential
    case "Y" => ExistentialVerbal
    case "#" => HashTag
    case "@" => Handle
    case "~" => DiscourseMarker
    case "U" => Url
    case "E" => Emoticon
    case "$" => Numeral
    case "," => Punctuation
    case _ => Garbage
  } //translateTokenType

  private def translateSerializedTokenType(typeStr: String): TokenType = typeStr match {
    case "CommonNoun" => CommonNoun
    case "Pronoun" => Pronoun
    case "PorperNoun" => PorperNoun
    case "NominalPossessive" => NominalPossessive
    case "ProperPossessive" => ProperPossessive
    case "Verb" => Verb
    case "NominalVerbal" => NominalVerbal
    case "ProperNounVerbal" => ProperNounVerbal
    case "Adjective" => Adjective
    case "Adverb" => Adverb
    case "Interjection" => Interjection
    case "Determiner" => Determiner
    case "PrePostPosition" => PrePostPosition
    case "Conjunction" => Conjunction
    case "VerbParticle" => VerbParticle
    case "Existential" => Existential
    case "ExistentialVerbal" => ExistentialVerbal
    case "HashTag" => HashTag
    case "Handle" => Handle
    case "DiscourseMarker" => DiscourseMarker
    case "Url" => Url
    case "Emoticon" => Emoticon
    case "Numeral" => Numeral
    case "Punctuation" => Punctuation
    case _ => Garbage
  } //translateSerializedTokenType

  /** pos tagger to use to compute tweets POS tags */
  val POSTagger: Tagger = {
    val tagger = new Tagger()
    tagger.loadModel("/cmu/arktweetnlp/model.20120919")
    tagger
  }

  private def posTagTweet(text: String) = {
    val sentence = new Sentence()
    sentence.tokens = Twokenize.tokenizeRawTweetText(text)
    sentence.T() // sentence size
    if (sentence.T() > 0) {
      val modelSentence = new ModelSentence(sentence.T())
      POSTagger.featureExtractor.computeFeatures(sentence, modelSentence)
      POSTagger.model.greedyDecode(modelSentence, true)
      val tokens = sentence.tokens.zipWithIndex.map { case (token, idx) =>
        (token, POSTagger.model.labelVocab.name(modelSentence.labels(idx)))
      }
      tokens
    } else
      Seq[(String, String)]()
  } //posTagTweet


  def apply(originalText: String) = {
    val tokensPos = posTagTweet(originalText)
    // computing offsets of tokens in originalText
    val acc = ArrayBuffer[Token]()
    var currentIdx = 0
    for ((token, pos) <- tokensPos) {
      val startOffset = originalText.indexOf(token, currentIdx)
      // html entities are converted in characters. Better to ignore things we cannot find
      if (startOffset >= 0) {
        val endOffset = startOffset + token.length
        acc += Token(token, startOffset, endOffset, translateTokenType(token, pos))
        currentIdx = endOffset
      }
    } //for
    new Tweet(originalText, acc.result())
  } //apply

}//xi.armatweet.nlp.Tweet