package xi.armatweet.nlp

import edu.stanford.nlp.ling.{CoreLabel, IndexedWord}
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.trees.{TypedDependency, UniversalEnglishGrammaticalRelations}
import org.json4s.DefaultFormats
import org.json4s.JsonAST.JString
import org.json4s.jackson.JsonMethods

import scala.collection.JavaConversions._
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer

/**
  * Created by alberto on 21/09/16.
  */

object CoreNLPJsonParse {
  implicit val formats = DefaultFormats

  case class DependencyEntry(dep: String,
                             governor: Int, governorGloss: String,
                             dependent: Int, dependentGloss: String)

  case class OpenIeEntryParsed(subject: String, subjectSpan: Seq[Int],
                               relation: String, relationSpan: Seq[Int],
                               `object`: String, objectSpan: Seq[Int])


  case class OpenIeEntry(subject: String, subjectSpan: (Int, Int),
                         relation: String, relationSpan: (Int, Int),
                         `object`: String, objectSpan: (Int, Int))


  case class TokenEntry(index: Int, word: String, originalText: String, lemma: String,
                        characterOffsetBegin: Int, characterOffsetEnd: Int,
                        pos: String, ner: String, before: String, after: String)

  private case class JsonSentenceParsed(index: Int, parse: String,
                                `basic-dependencies`: Seq[DependencyEntry],
                                `collapsed-dependencies`: Seq[DependencyEntry],
                                `collapsed-ccprocessed-dependencies`: Seq[DependencyEntry],
                                openie: Seq[OpenIeEntryParsed],
                                tokens: Seq[TokenEntry])

  case class JsonSentence(index: Int, parse: String,
                          `basic-dependencies`: Seq[DependencyEntry],
                          `collapsed-dependencies`: Seq[DependencyEntry],
                          `collapsed-ccprocessed-dependencies`: Seq[DependencyEntry],
                          openie: Seq[OpenIeEntry],
                          tokens: Map[Int, CoreLabel])

  case class JsonEntryPoint(sentences: Seq[JsonSentence])

  private case class JsonEntryPointParsed(sentences: Seq[JsonSentenceParsed])


  def buildSemanticGraph(sentenceParsed: JsonSentence, collapsed: Boolean) = {
    val depEntries = if (collapsed)
      sentenceParsed.`collapsed-dependencies`
    else
      sentenceParsed.`basic-dependencies`
    val depEntries_noRoots = depEntries.filterNot(_.dep == "ROOT")
    val coreLabels = sentenceParsed.tokens

    val roots = depEntries.filter(_.dep == "ROOT").map(x => new IndexedWord(coreLabels(x.dependent)))

    val dependencies = (new ListBuffer[TypedDependency]() /: depEntries_noRoots) { case (allDeps, depEntry) =>
      val depType = {
        val dt =
          if (depEntry.dep.contains("agent")) // nmod:agent is not recognised, for some reason
            UniversalEnglishGrammaticalRelations.valueOf("agent")
          else UniversalEnglishGrammaticalRelations.valueOf(depEntry.dep)
        if (dt == null) UniversalEnglishGrammaticalRelations.SEMANTIC_DEPENDENT
        else dt
      }
      //      if(depType == null)
      //        println(s"${depEntry.dep} has no correspondance")
      val dep = new TypedDependency(depType,
        new IndexedWord(coreLabels(depEntry.governor)), new IndexedWord(coreLabels(depEntry.dependent)))
      allDeps += dep
    }
    val sg = new SemanticGraph(dependencies)
    sg.setRoots(roots)
    sg
  } //buildSemanticGraph

  /**
    * Converts the tokens of a sentence into CoreLabels
    *
    * @param tokens
    * @return
    */
  def makeCoreLabels(tokens: Seq[TokenEntry]): IndexedSeq[CoreLabel] = {
    tokens.map { tok =>
      val cl = new CoreLabel(15)
      cl.setIndex(tok.index)
      cl.setWord(tok.word)
      cl.setOriginalText(tok.originalText)
      cl.setLemma(tok.lemma)
      cl.setBeginPosition(tok.characterOffsetBegin)
      cl.setEndPosition(tok.characterOffsetEnd)
      cl.setTag(tok.pos)
      cl.setNER(tok.ner)
      cl.setBefore(tok.before)
      cl.setAfter(tok.after)
      cl.setValue(tok.word)
      cl
    }.toIndexedSeq
  } //makeCoreLabels


  def parseJsonString(coreNLPJsonString: String): JsonEntryPoint = {
    val parsed = JsonMethods.parse(coreNLPJsonString).extract[JsonEntryPointParsed]

    val revisedSentences = parsed.sentences.map {
      case JsonSentenceParsed(index, parse, bdep, coldep, ccproc, openie, tokens) =>
        val coreLabels = makeCoreLabels(tokens).map(cl => cl.index() -> cl).toMap
        val openIeRightIndexes = openie.map {
          case OpenIeEntryParsed(s, sspan, rel, relSpan, obj, objSpan) =>
            // indexes are 0 based instead of 1 based as in the core-labels
            OpenIeEntry(s, (sspan(0) + 1, sspan(1) + 1),
              rel, (relSpan(0) + 1, relSpan(1) + 1),
              obj, (objSpan(0) + 1, objSpan(1) + 1)
            )
        }
        JsonSentence(index, parse, bdep, coldep, ccproc, openIeRightIndexes, coreLabels)
    }
    JsonEntryPoint(revisedSentences)
  } //parseJsonString


  def parseFromOuterJson(outerJson: String, field: String = "openIEjson") = {
    val outer = JsonMethods.parse(outerJson)
    val JString(inner) = outer \ field
    parseJsonString(inner)
  }

}

//CoreNLPJsonParse
