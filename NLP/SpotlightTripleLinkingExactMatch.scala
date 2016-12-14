package spark.tripleextraction

import nlputils.SpotlightJsonParse
import tweetsUtils.Tweet

/**
  * Created by alberto on 10/10/16.
  */
class SpotlightTripleLinkingExactMatch(val tw: Tweet, val spotlightResponse: String, val verbsLinkedStr: String) {


  // Seq[(uri, startOffset, endOffset_exclusive)]
  val spotlightEntities: Seq[(String, Int, Int)] = {
    val parsed = SpotlightJsonParse.parseSpotlightString(spotlightResponse)
    parsed match {
      case Some(matcher) => matcher.resources.map { entity =>
        //offsets are relative to the modified version of the string
        tw.toOriginalStringOffset(entity.begin, entity.end) match {
          case Some((s, e)) => Some((entity.uri, s, e))
          case None => None
        }
      }.filter(_.nonEmpty)
        .map(_.get)
      case None => Seq[(String, Int, Int)]()
    }
  }

  // Seq[(uri, startOffset, endOffset_exclusive)]
  val verbsLinked: Seq[(String, String, Int, Int)] = {
    // offsets are already OK :)

    //"spread\tVerb\tV\t<http://wordnet-rdf.princeton.edu/wn31/201381221-v>\t82\t88"
    //    println(verbsLinkedStr)
    if (verbsLinkedStr.nonEmpty)
      verbsLinkedStr.trim.split("\t").sliding(6, 6).map {
        case Array(mention, _, _, synset, startStr, endStr) =>
          //(mention, startStr.toInt, endStr.toInt)
          (tw.originalText.substring(startStr.toInt, endStr.toInt), synset, startStr.toInt, endStr.toInt)
      }.toSeq
    else Seq[(String, String, Int, Int)]()
  }

  val hasInfo = spotlightEntities.nonEmpty && verbsLinked.nonEmpty

  def sameOffset(mentionS: Int, mentionE: Int, linkedItem: (String, Int, Int)) = {
    mentionS == linkedItem._2 && mentionE == linkedItem._3 //we can relax the condition a bit…
  }

  def offsetsOverlap(mentionS: Int, mentionE: Int, linkedItem: (String, Int, Int)) =
    linkedItem._2 >= mentionS && linkedItem._3 <= mentionE


  def linkEntity(mention: String, startIdx: Int, endIdx: Int) = {
    spotlightEntities.find { linkedItem => sameOffset(startIdx, endIdx, linkedItem) }
  }

  def linkVerb(mention: String, startIdx: Int, endIdx: Int) = {
    val found = verbsLinked.find(linkedItem => mention.contains(linkedItem._1))
    found match {
      case Some((_, synset, s, e)) => Some((synset, s, e))
      case None => None
    }
  }//linkVerb

}

//SpotlightTripleLinkingExactMatch
