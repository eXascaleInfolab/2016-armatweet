package spark.tripleextraction

import edu.stanford.nlp.ling.{CoreAnnotations, CoreLabel}
import edu.stanford.nlp.simple.Document
import org.apache.spark.sql.functions._
import org.apache.spark.sql.{SQLContext, SaveMode}
import org.apache.spark.{SparkConf, SparkContext}
import tweetsUtils.Tweet

import scala.collection.JavaConversions._

/**
  * Created by alberto on 16/08/16.
  */
object SparkOpenIE {

  def getOffsets(tweet: Tweet, comp: Seq[CoreLabel]) = {

    if (comp.nonEmpty) {
      val cs = comp.head.get(classOf[CoreAnnotations.CharacterOffsetBeginAnnotation])
      val ce = comp.last.get(classOf[CoreAnnotations.CharacterOffsetEndAnnotation])
      tweet.toOriginalStringOffset_roundedTokens(cs, ce).getOrElse((-1, -1))
    } else (-1, -1)
  } //getOffsets


  //  def annotateTweet(r: Row, pipeline: StanfordCoreNLP): String = {
  //    val originalText = r.getAs[String]("text")
  //    val tokenizedText = r.getAs[String]("tweetNLPTokens")
  //    val tweetNLPos = r.getAs[String]("tweetNLPPosTags")
  //    val tweet = Tweet(originalText, tokenizedText, tweetNLPos)
  //    val cleanedTweet = tweet.clean()
  //    val cleanedTweet_str = cleanedTweet.text
  //
  //    try {
  //      if (cleanedTweet_str.trim.isEmpty) return ""
  //
  //      val doc = new Annotation(cleanedTweet_str)
  //      pipeline.annotate(doc)
  //
  //      val sentences = doc.get(classOf[SentencesAnnotation])
  //      val acc = new StringBuilder()
  //      for (sentence <- sentences) {
  //        val triples = sentence.get(classOf[NaturalLogicAnnotations.RelationTriplesAnnotation])
  //        for (triple <- triples) {
  //
  //          // convert to offsets in the original string
  //          val (ss, se) = getOffsets(tweet, triple.subject)
  //          val (ps, pe) = getOffsets(tweet, triple.relation)
  //          val (os, oe) = getOffsets(tweet, triple.`object`)
  //
  //          //          val s = if (ss >= 0) tweet.originalText.substring(ss, se) else "N/A"
  //          //          val p = if (ps >= 0) tweet.originalText.substring(ps, pe) else "N/A"
  //          //          val o = if (os >= 0) tweet.originalText.substring(os, oe) else "N/A"
  //          //          acc ++= s"\n${triple.confidence}\t$s\t$ss\t$se\t$p\t$ps\t$pe\t$o\t$os\t$oe"
  //          acc ++= s"\n${triple.confidence}\t$ss\t$se\t$ps\t$pe\t$os\t$oe"
  //        } //for triples
  //      } //for sentence
  //      return acc.result().trim
  //    } catch {
  //      case e: Throwable =>
  //        return "failed\n" + e.getMessage
  //    }
  //  } //annotateTweet


  def annotateTweet_simple(cleanedTweet_str: String): String = {
    //  (originalText: String, tokenizedText: String,
    //   tweetNLPos: String): String = {
    //    val tweet = Tweet(originalText, tokenizedText, tweetNLPos)
    //    val cleanedTweet = tweet.clean()
    //    val cleanedTweet_str = cleanedTweet.text

    try {
      val doc: Document = new Document(cleanedTweet_str)

      if (cleanedTweet_str.trim.isEmpty) return ""

      for (s <- doc.sentences()) {

        val x = s.nerTags()
        val y = s.openie()
      }
      doc.jsonMinified()
    } catch {
      case e: Throwable =>
        "failed\n" + e.getMessage
    } // try-catch

  } //annotateTweet


  def cleanTweetSerialize(originalText: String, tokenizedText: String,
                          tweetNLPos: String) = {

    val tweet = Tweet(originalText, tokenizedText, tweetNLPos)
    val cleanedTweet = tweet.clean()
    cleanedTweet.serializeStr
  } //cleanTweetSerialize


  def cleanTweetString(originalText: String, tokenizedText: String,
                       tweetNLPos: String) = {

    val tweet = Tweet(originalText, tokenizedText, tweetNLPos)
    val cleanedTweet = tweet.clean()
    cleanedTweet.text
  } //cleanTweetSerialize


  def main(args: Array[String]) {
    val conf = new SparkConf()
      .setAppName("SparkOpenIE")
      .set("spark.sql.parquet.compression.codec", "snappy")
//      .setMaster("local[1]")

    val sc = new SparkContext(conf)
    val sqlContext = new SQLContext(sc)

    val inputFile = args(0)
    val outputFile = args(1)

    //    val inputFile = "/Users/alberto/Documents/Projects/ArmaSuisse/tmp/bastard.json" //args(0)
    //    val inputFile = "/Users/alberto/Documents/Projects/ArmaSuisse/tmp/bastard_openie.json" //args(1)
    //    val outputFile = "/Users/alberto/Documents/Projects/ArmaSuisse/tmp/bastard_openie2.json" //args(1)

    val fcleanSer = udf((x: String, y: String, z: String) => cleanTweetSerialize(x, y, z))
    val fcleanString = udf((x: String, y: String, z: String) => cleanTweetString(x, y, z))
    val fopenIe = udf((x: String) => annotateTweet_simple(x))

    val tweetsAnnotated = sqlContext.read.parquet(inputFile).
      withColumn("cleanedTweet", fcleanSer(col("text"), col("tweetNLPTokens"), col("tweetNLPPosTags"))).
      withColumn("cleanedTweet_str", fcleanString(col("text"), col("tweetNLPTokens"), col("tweetNLPPosTags"))).
      withColumn("openIEjson", fopenIe(col("cleanedTweet_str")))

    tweetsAnnotated.write
//      .mode(SaveMode.Overwrite)
      .parquet(outputFile)
  } //main

}//SparkOpenIE
