package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.cache._
import scala.concurrent.{ Future, ExecutionContext }

import java.io.File

import scala.util.control.Breaks._
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
import org.clulab.processors.{ Document, Processor }
import ai.lum.odinson.extra.utils.{ ExtraFileUtils, ProcessorsUtils }
import ai.lum.odinson.extra.utils.ProcessorsUtils.getProcessor
import scala.collection.mutable._

import ai.lum.odinson._
import ai.lum.odinson.lucene.search._
import ai.lum.common.DisplayUtils._
import ai.lum.odinson.lucene._
import ai.lum.odinson.lucene.search.highlight.HtmlHighlighter

import engine._
import engine.QueryExceptions._

/** This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject() (
  val cache: SyncCacheApi,
  val controllerComponents: ControllerComponents
) extends BaseController {

  println("Warming up Engine, please wait...")
  // raw scala service designed to interact with py code
  // Specify paths and settings in the local config file
  val debug = false
  val print_query = true
  val config = ConfigFactory.load()
  val outputFile = config.apply[File]("odinson.extra.outputFile")
  val rulesFile = config.apply[String]("odinson.extra.rulesFile")
  val displayField = config.apply[String]("odinson.displayField")
  println(outputFile)
  println(System.getProperty("user.dir"))

  // Initialize the extractor engine, using the index specified in the config

  val extractorEngine = ExtractorEngine.fromConfig()
  // val proc: Processor = new FastNLPProcessor()
  val processorType = config.apply[String]("odinson.extra.processorType")
  val proc: Processor = getProcessor(processorType)
  proc.annotate("Warm up")

  println("Initialization Complete. Ready for execution.")

  /** Create an Action to render an HTML page.
    *
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.search())
  }

  /** Action to process and get results * */
  def results(queryString: String) = Action { implicit request: Request[AnyContent] =>
    // try {
    val query: Query = new Query(queryString)
    query.preProcess(proc)
    query.search(proc, extractorEngine)
    val (resultText, resultDoc, resultTitle) =
      query.generateResult(10, extractorEngine, proc, displayField)
    cache.set("RunningQuery", query)
    cache.set("ResultText", resultText)
    cache.set("ResultDoc", resultDoc)
    cache.set("ResultTitle", resultTitle)
    Ok(views.html.result(resultText.toList, resultDoc.toList, resultTitle.toList, queryString))
    // } catch {
    //   // if the exception is non-fatal then display it and keep going
    //   case NonFatal(e) => (Ok(views.html.search()))
    //   // case EmptyQueryException     => (Ok(views.html.search()))
    //   // case InvalidQueryException   => (OK(views.html.search()))
    //   // case SearchNotReadyException => (OK(views.html.search()))
    // }
  }

  def getMoreResults() = Action { implicit request: Request[AnyContent] =>
    val queryOption = cache.get[Query]("RunningQuery")
    val resultTextOption = cache.get[ListBuffer[String]]("ResultText")
    val resultDocOption = cache.get[ListBuffer[String]]("ResultDoc")
    val resultTitleOption = cache.get[ListBuffer[String]]("ResultTitle")
    if (
      queryOption.nonEmpty && resultTextOption.nonEmpty && resultDocOption.nonEmpty && resultTitleOption.nonEmpty
    ) {
      val query = queryOption.get
      var resultText = resultTextOption.get
      var resultDoc = resultDocOption.get
      var resultTitle = resultTitleOption.get
      val (nextText, nextDoc, nextTitle) =
        query.generateResult(10, extractorEngine, proc, displayField)
      resultText ++= nextText
      resultDoc ++= nextDoc
      resultTitle ++= nextTitle
      cache.set("RunningQuery", query)
      print(resultText.length)
      Ok(views.html.result(
        resultText.toList,
        resultDoc.toList,
        resultTitle.toList,
        query.querySentence
      ))
    } else {
      Ok(views.html.search())
    }
  }

}
