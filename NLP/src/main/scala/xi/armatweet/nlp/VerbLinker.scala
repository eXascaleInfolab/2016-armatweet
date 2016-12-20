package xi.armatweet.nlp

import edu.stanford.nlp.ie.util.RelationTriple
import edu.stanford.nlp.ling.{CoreAnnotations, IndexedWord}
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.trees.UniversalEnglishGrammaticalRelations
import net.sf.extjwnl.data.{POS, Synset}
import net.sf.extjwnl.dictionary.Dictionary

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

/**
  * Created by alberto on 20/05/16.
  */
class VerbLinker {

  private val dict = Dictionary.getDefaultResourceInstance

  private val NtRE = "n['`’]t$".r

  private val WordRE = "[\\w '`’]+".r

  def bestSynsetFor(verb: String): Option[(String, Synset)] = {
    val wnWord_tmp = dict.getIndexWord(POS.VERB, verb)
    if (wnWord_tmp == null) {
      None
    } else {
      wnWord_tmp.sortSenses()
      val bestSense = wnWord_tmp.getSenses
      if (bestSense.isEmpty)
        None
      else {
        Some((wnWord_tmp.getLemma, bestSense(0)))
      }
    } //if

  } //bestSynsetFor

  def linkVerb(tweetWithPOS: Array[(String, String)], idx: Int): Option[(String, Synset)] = {
    val v = tweetWithPOS(idx)._1
    val baseVerb = NtRE.replaceAllIn(v, "")

    val currentBest = bestSynsetFor(baseVerb)
    val bestSynset = currentBest match {
      //composite verb?
      case Some((verbMorphy, oneWordSset)) if (idx + 1 < tweetWithPOS.length && tweetWithPOS(idx + 1)._2 == "T") =>

        val compositeVerb = verbMorphy + "_" + tweetWithPOS(idx + 1)._1

        val compositeSynset = bestSynsetFor(compositeVerb)

        if (compositeSynset.nonEmpty)
          compositeSynset
        else {
          Some((verbMorphy, oneWordSset))
        }

      // not composite this time
      case Some(verbLinked) =>
        Some(verbLinked)

      case None => None
    } //case
    bestSynset
  } //linkVerb


  def isAuxiliaryVerb(iw: IndexedWord, semanticGraph: SemanticGraph): Boolean = {
    val parent = semanticGraph.getParent(iw)
    if (parent == null)
      false
    else {
      val relation = semanticGraph.getEdge(parent, iw).getRelation
      relation == UniversalEnglishGrammaticalRelations.AUX_MODIFIER ||
        relation == UniversalEnglishGrammaticalRelations.AUX_PASSIVE_MODIFIER ||
        relation == UniversalEnglishGrammaticalRelations.COPULA
    }
  } //isAuxiliaryVerb


  /**
    *
    * @param verb a non auxiliary verb (see isAuxiliaryVerb)
    * @param semanticGraph
    * @return an entry ((offset start, offset end), option(synset))
    *         IMPORTANT: the offset refer to the cleaned version of the tweet
    *         and need to be converted.
    */
  def linkVerb_coreNLP(verb: IndexedWord, semanticGraph: SemanticGraph) = {
    val phrasalParticles = semanticGraph.getChildrenWithReln(verb,
      UniversalEnglishGrammaticalRelations.PHRASAL_VERB_PARTICLE)

    // also check adverbial modifiers for phrasal verbs
    //    phrasalParticles = if (phrasalParticles.nonEmpty) phrasalParticles
    //    else
    //      semanticGraph.getChildrenWithReln(verb, UniversalEnglishGrammaticalRelations.ADVERBIAL_MODIFIER)

    val verbMorphy = verb.lemma()

    val phrasalLinked = phrasalParticles.map { phrasal =>
      val offset = if (phrasal.index() - verb.index() == 1)
        (verb.beginPosition(), phrasal.endPosition())
      else
        (verb.beginPosition(), verb.endPosition())

      val phrasalVerb = verbMorphy + "_" + phrasal.lemma()
      //      println(s"trying to link '$phrasalVerb'")
      val bestSynset = bestSynsetFor(phrasalVerb)
      //      if (bestSynset.nonEmpty) println("success")
      //      else println("failed")

      (offset, bestSynset)
    }.filter(_._2.nonEmpty)
    // if some phrasal version of the verb was linked, return it (there should be only one)
    // otherwise, link the base verb.
    if (phrasalLinked.nonEmpty) {
      phrasalLinked.head
    } else {
      ((verb.beginPosition(), verb.endPosition()), bestSynsetFor(verbMorphy))
    } //if
  } //linkVerb_coreNLP


  /**
    * offsets are relative to the original version of the tweet.
    *
    * @param tweet
    * @param semanticGraph
    * @return
    */
  def linkVerbs_offsets_coreNLP(tweet: Tweet, semanticGraph: SemanticGraph): IndexedSeq[Option[(Int, Int, String, Synset)]] = {
    val allVerbs = semanticGraph.getAllNodesByPartOfSpeechPattern("V.*")
    // don't link auxiliary verbs
    val toLink = allVerbs.filterNot(iw => isAuxiliaryVerb(iw, semanticGraph))

    val linked = toLink
      .map { verb => linkVerb_coreNLP(verb, semanticGraph) }
      .filter(_._2.nonEmpty)

    val withOffsetConverted = linked.map { case ((offB, offE), Some((str, synset))) =>
      //      println(s"cleaned tweet offsets: $offB, $offE")
      val convertedOffset = tweet.toOriginalStringOffset(offB, offE)
      convertedOffset match {
        case Some((b, e)) => Some(b, e, str, synset)
        case None => Some(-1, -1, str, synset)
      }
    }
    withOffsetConverted.toIndexedSeq
  } //linkVerbs_offset_coreNLP


  /**
    * Links a relation returned by the stanford OpenIE annotator
    *
    * @param triple
    */
  def linkRelation(triple: RelationTriple): Option[(String, Synset)] = {
    val verbs = triple.relation
      .map { cr => (cr, cr.get(classOf[CoreAnnotations.PartOfSpeechAnnotation])) }
      .filter(_._2.startsWith("V"))
    if (verbs.isEmpty) return None

    assert(verbs.length < 2)
    //assume there is only one verb
    val verb = verbs.head._1
    val baseVerb = if (verb.lemma() == null) NtRE.replaceAllIn(verb.originalText(), "") else verb.lemma()
    if (triple.asDependencyTree().isPresent) {
      // use the dependency graph to extracth phrasal verbs (e.g. "passed away")
      val dependencies = triple.asDependencyTree().get()

      val relationIndexes = triple.relation.map(_.index()).toSet
      // do lemmatization
      //        val currentBest = bestSynsetFor(baseVerb)
      val verbNode = dependencies.getNodeByIndex(verb.index())
      val phrasalVerbs = dependencies.getChildren(verbNode)
        .filter(w => relationIndexes.contains(w.index()))
        .map { w => baseVerb + "_" + w.originalText() }

      val phrasalSynsets = phrasalVerbs.map { pv => bestSynsetFor(pv) }.filter(_.nonEmpty)

      if (phrasalSynsets.nonEmpty) phrasalSynsets.head
      else bestSynsetFor(baseVerb)

    } else {
      // just link the first verb
      bestSynsetFor(baseVerb)
    } //if

  } //linkRelation


  def rdfWordnetId(synset: Synset): String = {
    f"<http://wordnet-rdf.princeton.edu/wn31/2${
      synset.getOffset
    }%08d-v>"
  } //rdfWordnetId

  val RDFSynsetRE = "<?http://wordnet-rdf.princeton.edu/wn31/2(\\d+)-v>?".r

  def rdfIdToSynset(wnetUri: String): Synset = {
    wnetUri match {
      case RDFSynsetRE(offset) =>
        dict.getSynsetAt(POS.VERB, offset.toLong)
    }
  }


  /**
    *
    * @param tweetWithPOS
    * @return a list of pairs (lemma_verb, synset), where
    *         lemma_verb is the lemmatised version of the surface
    *         form of the verb.
    */
  def linkVerbsInTweet(tweetWithPOS: Array[(String, String)]) = {
    val allVerbs = ListBuffer[(String, String)]()
    val verbsIdx = tweetWithPOS
      .zipWithIndex.filter {
      case ((text, pos), idx) => pos == "V" && (text match {
        case WordRE() => true
        case _ => false
      })
    }

    for (((v, pos), idx) <- verbsIdx) {
      val synset = linkVerb(tweetWithPOS, idx)
      synset match {
        case Some((processedVerb, synset)) =>
          allVerbs += processedVerb -> rdfWordnetId(synset)
        case None => // do nothing
      }
    } //for
    allVerbs.result()
  } //linkVerbsInTweet

}

//xi.armatweet.nlp.VerbLinker
