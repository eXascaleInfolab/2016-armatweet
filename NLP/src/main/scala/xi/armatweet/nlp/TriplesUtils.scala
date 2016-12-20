package xi.armatweet.nlp

import edu.stanford.nlp.ie.util.RelationTriple
import edu.stanford.nlp.ling.{CoreLabel, IndexedWord}
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.simple.Sentence
import edu.stanford.nlp.trees.UniversalEnglishGrammaticalRelations
import xi.armatweet.nlp.CoreNLPJsonParse.{JsonSentence, OpenIeEntry}

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * Created by alberto on 04/10/16.
  */
object TriplesUtils {

  def hasAuxPassiveRelation(triple: OpenIeEntry, g: SemanticGraph, tokens: Map[Int, CoreLabel]): Boolean = {
    val (relSpanS, relSpanE) = triple.relationSpan
    (relSpanS until relSpanE).exists { relIdx =>
      val iw = new IndexedWord(tokens(relIdx))
      if(! g.containsVertex(iw)) false
      else {
        val auxPassMod = g.getChildrenWithReln(iw, UniversalEnglishGrammaticalRelations.AUX_PASSIVE_MODIFIER)
        auxPassMod.nonEmpty
      }//if
    }//exists
  }//hasAuxPassiveRelation


  def hasPassiveSubject(triple: OpenIeEntry, g: SemanticGraph, tokens: Map[Int, CoreLabel]) = {
    val (s, e) = triple.subjectSpan
    (s until e).exists { subjIdx =>
      val iw = new IndexedWord(tokens(subjIdx))
      if(!g.containsVertex(iw)) false
      else {
        val nsubjpass = g.getParentsWithReln(iw, UniversalEnglishGrammaticalRelations.NOMINAL_PASSIVE_SUBJECT)
        val csubjpass = g.getParentsWithReln(iw, UniversalEnglishGrammaticalRelations.CLAUSAL_PASSIVE_SUBJECT)
        nsubjpass.nonEmpty || csubjpass.nonEmpty
      }//if
    }//exists
  } //hasPassiveSubject

  def hasAgent(triple: OpenIeEntry, g: SemanticGraph, tokens: Map[Int, CoreLabel]) = {
    val (s, e) = triple.objectSpan
    (s until e).exists { objIdx => // there is an object token such that
      val iw = new IndexedWord(tokens(objIdx))
      if( !g.containsVertex(iw) ) false
      else {
        g.getParents(iw).exists { parent => // there is one of its parent with relation "agent"
          //          println(s"parent: $parent, iw: $iw")
          val relation = g.getEdge(parent, iw).getRelation
          val relNames = Set(relation.getShortName, relation.getLongName, relation.getSpecific)
          //          println(s"rel name= $relName")
          relNames.exists { relName => relName != null && relName.contains("agent") }
        } //exists
      }
    }
  } //hasAgent

  def exampleConvertPassiveForm(triple: OpenIeEntry, g: SemanticGraph, tokens: Map[Int, CoreLabel]): Option[String] = {

    val (relSpanS, relSpanE) = triple.relationSpan
    lazy val hasAuxPassiveRelation = (relSpanS until relSpanE).exists { relIdx =>
      val iw = new IndexedWord(tokens(relIdx))
      if(! g.containsVertex(iw)) false
      else {
        val auxPassMod = g.getChildrenWithReln(iw, UniversalEnglishGrammaticalRelations.AUX_PASSIVE_MODIFIER)
        auxPassMod.nonEmpty
      }//if
    }
    val isPassive = relSpanE - relSpanS > 1 && hasAuxPassiveRelation

    if (!isPassive) return None

    val passiveSubj = hasPassiveSubject(triple, g, tokens)
    val agent = hasAgent(triple, g, tokens)

    if (passiveSubj && agent) {
      Some(triple.`object` + " -> " + triple.relation + " -> " + triple.subject)
    } else if (passiveSubj) {
      Some(s"? -> ${triple.relation} -> ${triple.subject}")
    } else if (agent) {
      Some(s"${triple.`object`} -> ${triple.relation} -> ?")
    } else
      None
  } //invertPassiveForm


  /**
    *
    * @param objTokenCL
    * @param g
    * @param relationIdxs
    * @return a set of tuples (relation_token, object_token, relation)
    *         where relation_token depends has a "case" dependency with object_token
    *         (object_token is the governor).
    *         Relation can be either "TIME" or "LOCATION".
    */
  private def findDependencyPredObj(objTokenCL: CoreLabel, g: SemanticGraph, relationIdxs: Set[Int]) = {
    val objToken = new IndexedWord(objTokenCL)
    val objTokenNer = objToken.ner()
    val objTokenPos = objToken.tag()
    // all children of objToken contained in the relation
    val relDependents = if(g.containsVertex(objToken))
      g.getChildren(objToken).filter(objToken => relationIdxs.contains(objToken.index()))
    else
      mutable.Set[IndexedWord]()

    /* for the moment keeping only cases. I observed in a small part of the dataset
   * that all the relations which were interesting in this context were "case" relations.
   * There were also copula and aux dependencies but weren't interesting.
   */
    val caseRelations = relDependents.map { relDepIw =>
      (relDepIw, g.getEdge(objToken, relDepIw).getRelation)
    }.filter(_._2 == UniversalEnglishGrammaticalRelations.CASE_MARKER)

    // analyse ner and pos of the object to see which kind of relation it is
    val timeLocationRelations = caseRelations.map { case (relDepIw, relation) =>
      if (objTokenNer == "LOCATION") {
        //triple specifies location
        (relDepIw, objToken, "LOCATION")
      } else if (Set("DATE", "TIME").contains(objTokenNer) || objTokenPos == "CD") {
        (relDepIw, objToken, "TIME")
      } else {
        (relDepIw, objToken, "")
      } //if
    }.filterNot(_._3 == "")
    timeLocationRelations
  } //findDependencyPredObj

  /**
    * Returns
    *
    * @param triple
    * @param s
    * @return Some("TIME"/"LOCATION") if the triple is a time
    *         or location triple. None otherwise.
    */
  def isTimeLocationTriple(triple: RelationTriple, s: Sentence): Option[String] = {
    val obj = triple.`object`
    val relation = triple.relation
    if (triple.relationLemmaGloss().contains("at_time"))
      return Some("TIME")

    val g = s.dependencyGraph()
    s.nerTags() // just to populate the .ner field of coreLabels
    val relationIdxs = relation.map(_.index()).toSet
    val dependenciesObjRel = obj.flatMap { objTok =>
      findDependencyPredObj(objTok, g, relationIdxs)
    }

    if (dependenciesObjRel.isEmpty) None
    else {
      val allTimeLocation = dependenciesObjRel.map(_._3)
        .groupBy(identity)
        .map { case (relation, entries) => (relation, entries.size) }
        .maxBy(_._2)._1
      if (allTimeLocation == "") None else Some(allTimeLocation)
    } //if
  } //isTimeLocationTriple


  /**
    * Returns
    *
    * @param triple
    * @param s
    * @return Some("TIME"/"LOCATION") if the triple is a time
    *         or location triple. None otherwise.
    */
  def isTimeLocationTriple(triple: OpenIeEntry, s: JsonSentence, g: SemanticGraph): Option[String] = {
    val obj = triple.`object`
    val relation = triple.relation
    if (triple.relation.contains("at_time"))
      return Some("TIME")

    val (relS, relE) = triple.relationSpan
    val (objS, objE) = triple.objectSpan

    val relationIdxs = (relS until relE).toSet
    val dependenciesObjRel = (objS until objE).flatMap { objIdx =>
      val objTok = s.tokens(objIdx)
      findDependencyPredObj(objTok, g, relationIdxs)
    }

    if (dependenciesObjRel.isEmpty) None
    else {
      val allTimeLocation = dependenciesObjRel.map(_._3)
        .groupBy(identity)
        .map { case (relation, entries) => (relation, entries.size) }
        .maxBy(_._2)._1
      if (allTimeLocation == "") None else Some(allTimeLocation)
    } //if
  } //isTimeLocationTriple


}

//object
