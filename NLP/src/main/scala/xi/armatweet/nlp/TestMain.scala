package xi.armatweet.nlp

import cmu.arktweetnlp.{Tagger, Twokenize}
import cmu.arktweetnlp.impl.{ModelSentence, Sentence}

import scala.collection.JavaConversions._

/**
  * Created by alberto on 19.12.16.
  */
object TestMain {




  def main(args: Array[String]): Unit = {
    val tweetStr =  "goin for lunch with my colleages! We <3 mensa! #unifr #mensa @cacca"
    val tw = Tweet(tweetStr)
    tw.clean()
  }

}//TestMain
