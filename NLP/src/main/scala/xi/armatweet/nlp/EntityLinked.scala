package xi.armatweet.nlp

/**
  * Created by alberto on 19.12.16.
  */
case class EntityLinked(mention: String, etype: String, entityUri: String, start: Int, end: Int, entropy: Double)
