name := "NLP"

version := "1.0"

scalaVersion := "2.10.6"

// Stanford CoreNLP & models
libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0"

libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0" classifier "models"

libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.0.0"


// Wordnet API
libraryDependencies += "net.sf.extjwnl" % "extjwnl" % "1.9.1"

libraryDependencies += "net.sf.extjwnl" % "extjwnl-data-wn31" % "1.2"

libraryDependencies += "org.apache.lucene" % "lucene-core" % "6.1.0"

libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.2.1"

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.4.1"
