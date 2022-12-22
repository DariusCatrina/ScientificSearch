package ai.lum.odinson.extra

import java.io.File

import scala.util.control.NonFatal
import ai.lum.common.ConfigFactory
import ai.lum.common.ConfigUtils._
import ai.lum.common.FileUtils._
import ai.lum.odinson.serialization.JsonSerializer
import ai.lum.odinson.utils.DisplayUtils.displayMention
import ai.lum.odinson.utils.SituatedStream
import ai.lum.odinson.ExtractorEngine
import ai.lum.odinson.DataGatherer.VerboseLevels._
import com.typesafe.scalalogging.LazyLogging
import upickle.default._
import scala.io.StdIn.readLine
import java.io._
import org.clulab.struct.DirectedGraphEdgeIterator
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor}

object AutomaticShell extends App with LazyLogging {

  // raw scala service designed to interact with py code
  // Specify paths and settings in the local config file
  val config = ConfigFactory.load()
  val outputFile = config.apply[File]("odinson.extra.outputFile")
  val rulesFile = config.apply[String]("odinson.extra.rulesFile")

  // Initialize the extractor engine, using the index specified in the config
  val extractorEngine = ExtractorEngine.fromConfig()
  val proc: Processor = new CoreNLPProcessor()
  proc.annotate("Warm up")

  try {
    // run the shell
    var running = true
    while (running) {
      try {
        val line = readLine("Test")
        if (line == null) {
          println(":exit")
          running = false
        } else {
          line.trim match {
            case ":annotateQuery"  => 
              val querysentence = config.apply[String]("odinson.extra.rawQuery")
              val doc: Document = proc.annotate(querysentence)
              val pw = new PrintWriter(new File("test.txt" ))

              for ((sentence, sentenceIndex) <- doc.sentences.zipWithIndex) {
                sentence.dependencies.foreach { dependencies =>
                    val iterator = new DirectedGraphEdgeIterator[String](dependencies)
                    iterator.foreach { dep =>
                    // Note that we use offsets starting at 0 unlike CoreNLP, which uses offsets starting at 1.
                    pw.write(dep._1 + " " + dep._2 + " " + dep._3 +"\n")
                    println(dep._1 + " " + dep._2 + " " + dep._3)
                    }
                }
                pw.close()
              }
              println("Finished. Query written to test.txt")
            case ":runQuery"      => 
              val rulesStream = SituatedStream.fromResource(rulesFile)
              val extractors = extractorEngine.ruleReader.compileRuleStream(rulesStream)
              println(s"Found ${extractors.length} extractors")

              // Extract Mentions
              val mentions = extractorEngine.extractMentions(extractors).toArray
              mentions.foreach(displayMention(_, extractorEngine))

              // Export Mentions (here as json lines)
              val jsonSerializer = {
                    // can choose several levels of verbosity: Minimal, Display, and All
                    new JsonSerializer(verbose = Display, dataGathererOpt = Some(extractorEngine.dataGatherer))
              }

              val serialized = jsonSerializer.asJsonLines(mentions)
              outputFile.writeString(serialized.mkString("\n"))
            case ":exit"      => running = false
          }
        }
      } catch {
        // if the exception is non-fatal then display it and keep going
        case NonFatal(e) => (e.printStackTrace())
      }
    }
  } finally {
    // manual terminal cleanup
  }
}