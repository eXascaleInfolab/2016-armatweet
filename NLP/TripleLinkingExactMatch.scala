package spark.tripleextraction

import edu.stanford.nlp.semgraph.SemanticGraph
import nlputils.CoreNLPJsonParse.{JsonSentence, OpenIeEntry}
import nlputils.{CoreNLPJsonParse, SpotlightJsonParse, TriplesUtils}
import org.apache.spark.sql.{SQLContext, SaveMode}
import org.apache.spark.sql.functions._
import org.apache.spark.{SparkConf, SparkContext}
import tweetsUtils.Tweet

import scala.collection.mutable

/**
  * Created by alberto on 10/10/16.
  */
object TripleLinkingExactMatch {

  case class LinkedTriple(subject: Option[(String, Int, Int)],
                          predicate: Option[(String, Int, Int)],
                          obj: Option[(String, Int, Int)],
                          location: Option[(String, Int, Int)],
                          time: Option[(String, Int, Int)])

  def linkTriple(tweet: Tweet, triple: OpenIeEntry,
                 sentence: JsonSentence,
                 graph: SemanticGraph,
                 linker: SpotlightTripleLinkingExactMatch) = {

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


  def extractTriplesFromTweet(tweetOriginalText: String,
                              tweetSerialised: String, coreNlpStr: String,
                              verbsStr: String, spotlightStr: String): String = {

    val tweet = Tweet(tweetOriginalText, tweetSerialised)
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
        val s = ss.getOrElse((TripleLinkingPartialMatch.NOT_LINKED, -1, -1))._1
        val p = ps.getOrElse((TripleLinkingPartialMatch.NOT_LINKED, -1, -1))._1
        val o = os.getOrElse((TripleLinkingPartialMatch.NOT_LINKED, -1, -1))._1
        val l = ls.getOrElse((TripleLinkingPartialMatch.NOT_LINKED, -1, -1))._1
        val t = ts.getOrElse((TripleLinkingPartialMatch.NOT_LINKED, -1, -1))._1
        s"$s\t$p\t$o\t$l\t$t"
      }.mkString("\t")
  } //extractTriplesFromTweet


  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setAppName("TripleLinkingExactMatch")
      .set("spark.sql.parquet.compression.codec", "snappy")
      .set("spark.task.maxFailures", "1")
    //      .setMaster("local[*]")

    val sc = new SparkContext(conf)
    val sqlContext = new SQLContext(sc)

    //    val input = "/Users/alberto/Documents/Projects/ArmaSuisse/test_data/example_spotlight_vrb_entities_openiejson.parquet"
    //    val output = "/Users/alberto/Documents/Projects/ArmaSuisse/test_data/example_spotlight_vrb_entities_openiejson_triplelinked.parquet"

    val input = args(0)
    val output = args(1)

    val f = udf((tweetOriginalText: String,
                 tweetSerialised: String, coreNlpStr: String,
                 verbsStr: String, spotlightStr: String) => extractTriplesFromTweet(tweetOriginalText, tweetSerialised, coreNlpStr, verbsStr, spotlightStr))

    val fSpotlight = udf((tweetOriginalText: String,
                          tweetSerialised: String, spotlightStr: String) => {
      val tweet = Tweet(tweetOriginalText, tweetSerialised)
      SpotlightJsonParse.toEntityString(tweet, spotlightStr)
    })

    val tweets = sqlContext.read.parquet(input)
      .drop("entities_entropy").drop("namedEntities") //TODO
      .where("openIEjson NOT LIKE 'failed%' AND " +
      "TRIM(spotlight05) <> '' AND " +
      "TRIM(linked_verbs) <> ''")

    val triplesLinked = tweets.withColumn("triples_linked_spotlight", f(
      col("text"), col("cleanedTweet"), col("openIEjson"), col("linked_verbs"), col("spotlight05")))

    val withSpotlightEntitiesStr = triplesLinked
      .withColumn("spotlight05str",
        fSpotlight(col("text"), col("cleanedTweet"), col("spotlight05")))

    //    withSpotlightEntitiesStr
    //      .where("spotlight05str <> '' OR triples_linked_spotlight <> ''")
    //      .select("id", "text", "spotlight05str", "triples_linked_spotlight")
    //      .write.mode(SaveMode.Overwrite).json(output)

    withSpotlightEntitiesStr.write.parquet(output)
  } //main
}

//TripleLinkingExactMatch
