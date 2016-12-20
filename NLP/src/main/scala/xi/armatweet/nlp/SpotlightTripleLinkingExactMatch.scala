package xi.armatweet.nlp



/**
  * Created by alberto on 10/10/16.
  */
class SpotlightTripleLinkingExactMatch(val tw: Tweet,
                                       val spotlightResponse: String,
                                       val verbsLinked: Seq[(String, String, Int, Int)]) {


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

  val hasInfo: Boolean = spotlightEntities.nonEmpty && verbsLinked.nonEmpty

  private def sameOffset(mentionS: Int, mentionE: Int, linkedItem: (String, Int, Int)) = {
    mentionS == linkedItem._2 && mentionE == linkedItem._3 //we can relax the condition a bit…
  }

  private def offsetsOverlap(mentionS: Int, mentionE: Int, linkedItem: (String, Int, Int)) =
    linkedItem._2 >= mentionS && linkedItem._3 <= mentionE


  def linkEntity(mention: String, startIdx: Int, endIdx: Int): Option[(String, Int, Int)] = {
    spotlightEntities.find { linkedItem => sameOffset(startIdx, endIdx, linkedItem) }
  }

  def linkVerb(mention: String, startIdx: Int, endIdx: Int): Option[(String, Int, Int)] = {
    val found = verbsLinked.find(linkedItem => mention.contains(linkedItem._1))
    found match {
      case Some((_, synset, s, e)) => Some((synset, s, e))
      case None => None
    }
  }//linkVerb

}//SpotlightTripleLinkingExactMatch