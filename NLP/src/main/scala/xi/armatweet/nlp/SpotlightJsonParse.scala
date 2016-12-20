package xi.armatweet.nlp

import org.json4s.DefaultFormats
import org.json4s.JsonAST.JString
import org.json4s.jackson.JsonMethods

/**
  * Created by alberto on 10/10/16.
  */
object SpotlightJsonParse {
  implicit val formats = DefaultFormats

  case class SpotlightEntityParse(`@URI`: String, `@support`: String, `@types`: String,
                                  `@surfaceForm`: String, `@offset`: String,
                                  `@similarityScore`: String, `@percentageOfSecondRank`: String)

  case class SpotlightFieldParse(`@text`: String, `@confidence`: String, `@support`: String,
                                 `@types`: String, `@sparql`: String, `@policy`: String, Resources: Seq[SpotlightEntityParse])

  case class SpotlightEntity(uri: String, support: Int, types: String,
                             surfaceForm: String, begin: Int, end: Int,
                             similarityScore: Double, percentageOfSecondRank: Double)

  case class SpotlightField(confidence: Double, support: Int, resources: Seq[SpotlightEntity])


  def parseSpotlightString(str: String) = {
    try {
      val entryParsed = JsonMethods.parse(str).extract[SpotlightFieldParse]

      val entities = entryParsed.Resources.map {
        case SpotlightEntityParse(uri, supportStr, types, surface, offsetStr, simStr, secRankStr) =>
          SpotlightEntity(s"<$uri>", supportStr.toInt, types, surface,
            offsetStr.toInt, offsetStr.toInt + surface.length, simStr.toDouble, secRankStr.toDouble)
      }

      Some(SpotlightField(entryParsed.`@confidence`.toDouble, entryParsed.`@support`.toInt, entities))
    } catch {
      case e: Exception => None
    }
  } //parseSpotlightString


  def fromOuterString(outerStr: String, spotlightField: String = "spotlight05") = {
    val j = JsonMethods.parse(outerStr)
    val JString(toParse) = j \ spotlightField
    parseSpotlightString(toParse)
  }//secRankStr


  def toEntityString(tweet: Tweet, spotlightStr: String): String = {
    parseSpotlightString(spotlightStr) match{
      case Some(se) =>
        val entities_str = (new StringBuilder("") /: se.resources) { case (builder, res) =>
          val (s, e) = tweet.toOriginalStringOffset(res.begin, res.end).getOrElse((-1, -1))

          builder ++= "%s\t%s\t*\t%s\t%d\t%d\t".
            format(res.surfaceForm, res.types, res.uri, s, e)
        }
        entities_str.result().trim

      case None => ""
    }//match
  }//toEntityString

}

//SpotlightJsonParse
