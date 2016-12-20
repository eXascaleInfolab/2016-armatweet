package xi.armatweet.nlp

import edu.stanford.nlp.simple.Document
import xi.armatweet.nlp.TripleLinkingExactMatch.LinkedTriple

import scala.collection.JavaConversions._
import scalaj.http.Http


/**
  * Created by alberto on 19.12.16.
  */
class QuadExtractor(tweetText: String) {


  private val cleanedTweet = Tweet(tweetText).clean()

  private val coreNLPDoc = new Document(cleanedTweet.text)

  private val coreNLPStr = {
    coreNLPDoc.sentences().foreach {
      x =>
        x.openie()
        x.nerTags()
    }
    coreNLPDoc.jsonMinified()
  }

  private val corenlpParsed = CoreNLPJsonParse.parseJsonString(coreNLPStr)

  //TODO: check for failure
  private val spotlightResponseStr = QuadExtractor.askSpotlight(cleanedTweet.text)

  private val verbsLinked = coreNLPDoc.sentences().flatMap { sentence =>
    QuadExtractor.verbLinker.linkVerbs_offsets_coreNLP(cleanedTweet, sentence.dependencyGraph())
  }.filter(_.nonEmpty).map { opt =>
    val (s, e, mention, synset) = opt.get
    (mention, QuadExtractor.verbLinker.rdfWordnetId(synset), s, e)
  }

  val tripleLinker = new SpotlightTripleLinkingExactMatch(cleanedTweet, spotlightResponseStr._1, verbsLinked)

  val linkedTriples: Seq[LinkedTriple] = corenlpParsed.sentences.flatMap { sentence =>
    val depGraph = CoreNLPJsonParse.buildSemanticGraph(sentence, collapsed = true)
    sentence.openie.map { triple =>
      TripleLinkingExactMatch.linkTriple(cleanedTweet, triple, sentence, depGraph, tripleLinker)
    }
  }


}//class


object QuadExtractor {

  private val SPOTLIGHT_SERVER = "http://spotlight.sztaki.hu:2222/rest/annotate"
  val verbLinker: VerbLinker = new VerbLinker()

  private def askSpotlight(txt: String, confidence: Double = 0.5, support: Int = 20) = {
    try {
      val request = Http(SPOTLIGHT_SERVER).
        timeout(connTimeoutMs = 10000, readTimeoutMs = 20000).
        header("Accept", "application/json").
        header("Accept-Charset", "utf-8").
        postForm(Seq("text" -> txt,
          "confidence" -> confidence.toString,
          "support" -> support.toString))
      (request.asString.body, false)
    } catch {
      case t: Throwable =>
        println(s"couldn't process '$txt'\n" + t.getMessage)
        ("", true)
    }
  } //askSpotlight

}

//companion