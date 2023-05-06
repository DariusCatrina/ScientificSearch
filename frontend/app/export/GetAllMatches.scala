package export

import ai.lum.common.ConfigFactory
import ai.lum.common.ConfigUtils._
import engine._
import engine.QueryExceptions._
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{ Document, Processor }
import java.io._
import ai.lum.odinson.ExtractorEngine
import ai.lum.odinson.extra.utils.{ ExtraFileUtils, ProcessorsUtils }
import ai.lum.odinson.extra.utils.ProcessorsUtils.getProcessor

object GetAllMatches extends App {
  val config = ConfigFactory.load()
  val extractorEngine = ExtractorEngine.fromConfig()
  val processorType = config.apply[String]("odinson.extra.processorType")
  val displayField = config.apply[String]("odinson.displayField")
  val proc: Processor = getProcessor(processorType)

  val query: Query = new Query(args(0))
  query.preProcess(proc)
  query.search(proc, extractorEngine)

  val (resultText, resultDoc, resultTitle, resultCount, resultCaptures) =
    query.generateResult(-1, extractorEngine, proc, displayField)

  val pw = new PrintWriter(new File("output.csv"))
  if (resultText.length > 0) {
    pw.write(s""""Raw Text", "${resultCaptures(0)._1}"\n""")
  }
  for (i <- 0 until resultText.length) {
    pw.write(s""""${resultText(i).replaceAll("<mark class=\"odin-mention\">", "").replaceAll(
      "</mark>",
      ""
    ).replaceAll("<mark class=\"odin-arg\">", "")}", "${resultCaptures(i)._2}"\n""")
  }
  pw.close()

}
