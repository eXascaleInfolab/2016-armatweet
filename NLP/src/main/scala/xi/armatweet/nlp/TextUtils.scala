package xi.armatweet.nlp

import scala.util.matching.Regex

/**
  * Created by alberto on 19.12.16.
  */
object TextUtils {

  val CamelCaseRE: Regex = "(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])".r
  val HandlesHashRE: Regex = "([\\@\\#])([\\w_]+)".r
  val DoubleSpaceRE: Regex = "\\s+".r
  val HttpUrlRE: Regex = "https?:\\S+".r
  val WordSeparatorRE: Regex = "[-_]+".r
  val EndingTagListRE: Regex = "(\\s+([\\@\\#][\\w_]+))(\\W*([\\@\\#][\\w_]+))+\\W*$".r

}
