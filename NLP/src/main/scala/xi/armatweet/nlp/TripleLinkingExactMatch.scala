package xi.armatweet.nlp

import edu.stanford.nlp.semgraph.SemanticGraph
import net.sf.extjwnl.data.Synset
import xi.armatweet.nlp.CoreNLPJsonParse.{JsonSentence, OpenIeEntry}

import scala.collection.mutable

/**
  * Created by alberto on 10/10/16.
  */
object TripleLinkingExactMatch {
  val NOT_LINKED = "?"

  case class LinkedTriple(subject: Option[(String, Int, Int)],
                          predicate: Option[(String, Int, Int)],
                          obj: Option[(String, Int, Int)],
                          location: Option[(String, Int, Int)],
                          time: Option[(String, Int, Int)])

  def linkTriple(tweet: Tweet, triple: OpenIeEntry,
                 sentence: JsonSentence,
                 graph: SemanticGraph,
                 linker: SpotlightTripleLinkingExactMatch): LinkedTriple = {

    val (relSpanS, relSpanE) = triple.relationSpan
    val isPassive = relSpanE - relSpanS > 1 && TriplesUtils.hasAuxPassiveRelation(triple, graph, sentence.tokens)
    val passiveSubj = TriplesUtils.hasPassiveSubject(triple, graph, sentence.tokens)
    val agent = TriplesUtils.hasAgent(triple, graph, sentence.tokens)

    val relationLinked = tweet.toOriginalStringOffset(
      sentence.tokens(triple.relationSpan._1).beginPosition(),
      sentence.tokens(triple.relationSpan._2 - 1).endPosition()) match {
      case Some((start, e)) => linker.linkVerb(triple.relation, start, e)
      case None => None
    }

    val subjectLinked = tweet.toOriginalStringOffset(
      sentence.tokens(triple.subjectSpan._1).beginPosition(),
      sentence.tokens(triple.subjectSpan._2 - 1).endPosition()) match {
      case Some((start, e)) => linker.linkEntity(triple.subject, start, e)
      case None => None
    }

    val objLinked = tweet.toOriginalStringOffset(
      sentence.tokens(triple.objectSpan._1).beginPosition(),
      sentence.tokens(triple.objectSpan._2 - 1).endPosition()) match {
      case Some((start, e)) => linker.linkEntity(triple.`object`, start, e)
      case None => None
    }

    TriplesUtils.isTimeLocationTriple(triple, sentence, graph) match {

      case Some("TIME") => // don't consider the object as it's a time component
        val (timeS, timeE) = (sentence.tokens(triple.objectSpan._1).beginPosition(),
          sentence.tokens(triple.objectSpan._2 - 1).endPosition())
        val timeStr = triple.`object`.replaceAll("\\s+", " ")

        if (isPassive && passiveSubj) {
          // is passive: subject should be the object
          //          println(s"TIME TRIPLE (P): (<?>, '${relationLinked}' ,  '${triple.subject}')")
          LinkedTriple(None, relationLinked, subjectLinked, None, Some((timeStr, timeS, timeE)))
        } else {
          // active form
          //          println(s"TIME TRIPLE (A): ('${subjectLinked}', '${relationLinked}', <?>)")
          LinkedTriple(subjectLinked, relationLinked, None, None, Some((timeStr, timeS, timeE)))
        } //if

      case Some("LOCATION") => // location triple: the object is the location
        if (isPassive && passiveSubj) {
          //          println(s"LOCATION TRIPLE (P): (<?>, '${relationLinked}' ,  '${subjectLinked}', IN '${objLinked}')")
          LinkedTriple(None, relationLinked, subjectLinked, objLinked, None)
        } else {
          //          println(s"LOCATION TRIPLE (A): (<?>, '${relationLinked}' ,  '${subjectLinked}', IN '${objLinked}')")
          LinkedTriple(subjectLinked, relationLinked, None, objLinked, None)
        } //if

      case None =>
        // normal triple

        if (isPassive && passiveSubj && agent) {
          //          println(s"NORMAL TRIPLE (O, P, S): ('${triple.`object`}', '${relationLinked}', '${subjectLinked}')")
          LinkedTriple(objLinked, relationLinked, subjectLinked, None, None)
        } else if (isPassive && passiveSubj) {
          //          println(s"NORMAL TRIPLE (?, P, S): (<?>, '${relationLinked}', '${subjectLinked}')")
          LinkedTriple(None, relationLinked, subjectLinked, None, None)
        } else if (isPassive && agent) {
          //          println(s"NORMAL TRIPLE (O, P, ?): ('${objLinked}' , '${relationLinked}', <?>)")
          LinkedTriple(objLinked, relationLinked, None, None, None)
        } else {
          //          println(s"NO/**/RMAL TRIPLE (S, P, O): ('${subjectLinked}', '${relationLinked}', '${objLinked}')")
          LinkedTriple(subjectLinked, relationLinked, objLinked, None, None)
        } //if
    } //match
  } //linkTriple


  def isGoodTriple(triple: LinkedTriple): Boolean = {
    // at least two components are linked
    def o2i[A](x: Option[A]) = if (x.isEmpty) 0 else 1
    o2i(triple.subject) + o2i(triple.predicate) +
      o2i(triple.obj) + o2i(triple.location) + o2i(triple.time) >= 2
  } //isGoodTriple


  def extractTriplesFromTweet(tweet: Tweet,
                              coreNlpStr: String,
                              verbsStr: Seq[(String, String, Int, Int)],
                              spotlightStr: String): String = {

    val corenlpJson = CoreNLPJsonParse.parseJsonString(coreNlpStr)
    val linker = new SpotlightTripleLinkingExactMatch(tweet, spotlightStr, verbsStr)
    val acc = mutable.ArrayBuffer[LinkedTriple]()
    if (!linker.hasInfo) return ""

    for (sentence <- corenlpJson.sentences) {
      val semGraph = CoreNLPJsonParse.buildSemanticGraph(sentence, collapsed = true)
      for (triple <- sentence.openie) {
        val linkedTriple = linkTriple(tweet, triple, sentence, semGraph, linker)
        acc += linkedTriple
      } //for
    } //for
    acc.filter(isGoodTriple)
      .map { case LinkedTriple(ss, ps, os, ls, ts) =>
        val s = ss.getOrElse((NOT_LINKED, -1, -1))._1
        val p = ps.getOrElse((NOT_LINKED, -1, -1))._1
        val o = os.getOrElse((NOT_LINKED, -1, -1))._1
        val l = ls.getOrElse((NOT_LINKED, -1, -1))._1
        val t = ts.getOrElse((NOT_LINKED, -1, -1))._1
        s"$s\t$p\t$o\t$l\t$t"
      }.mkString("\t")
  } //extractTriplesFromTweet



}
