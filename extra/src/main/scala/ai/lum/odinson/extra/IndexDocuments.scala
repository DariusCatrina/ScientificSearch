package ai.lum.odinson.extra

import java.io._
import scala.util.{ Failure, Success, Try }
import com.typesafe.scalalogging.LazyLogging
import ai.lum.common.ConfigFactory
import com.typesafe.config.{ Config, ConfigValueFactory }
import ai.lum.common.ConfigUtils._
import ai.lum.common.FileUtils._
import ai.lum.odinson.lucene.index.{ OdinsonIndex, OdinsonIndexWriter }
import ai.lum.odinson.{ Document, StringField }

import scala.collection.GenIterable

object IndexDocuments extends App with LazyLogging {

  var config = ConfigFactory.load()

  // Warn that the API requires parentDocFieldFileName
  val storedFields = config.apply[List[String]]("odinson.index.storedFields")
  val fileNameField = config.apply[String]("odinson.index.parentDocFieldFileName")
  if (!storedFields.contains(fileNameField)) {
    logger.warn(
      "`odinson.index.storedFields` must contain `odinson.index.parentDocFieldFileName` to enable the Odinson API"
    )
  }

  if (args.length == 1) {

    val dirPath = args(0)
    val passedInDataDir = new File(dirPath).getAbsolutePath
    val passedInIndexDir = new File(passedInDataDir, "index").getAbsolutePath
    val passedInDocsDir = new File(passedInDataDir, "docs").getAbsolutePath

    logger.info(s"Received dataDir as a parameter <${dirPath}>")
    // receive the path from the arguments
    config = config
      .withValue("odinson.dataDir", ConfigValueFactory.fromAnyRef(passedInDataDir))
      // re-compute the index and docs path's
      .withValue(
        "odinson.indexDir",
        ConfigValueFactory.fromAnyRef(passedInIndexDir)
      )
      .withValue(
        "odinson.docsDir",
        ConfigValueFactory.fromAnyRef(passedInDocsDir)
      )
  }
  //
  val docsDir = config.apply[File]("odinson.docsDir")

  val synchronizeOrderWithDocumentId =
    config.apply[Boolean]("odinson.index.synchronizeOrderWithDocumentId")

  //
  val index = OdinsonIndex.fromConfig(config)
  logger.info(s"Gathering documents from $docsDir")

  // make this a function
  val documentFiles =
    if (synchronizeOrderWithDocumentId) {
      // files ordered by the id of the document
      docsDir
        .listFilesByWildcards(odinsonDocsWildcards, recursive = true)
        .map(f => (Document.fromJson(f).id.toInt, f))
        .toSeq
        .sortBy(_._1)
        .map(_._2)
    } else {
      docsDir
        .listFilesByWildcards(odinsonDocsWildcards, recursive = true)
        .par
    }

  // ^ this part should be a function
  logger.info("Indexing documents")
  indexDocuments(documentFiles)
  index.close()

  // fin
  // Note that documentFiles may or may not be parallel, hence the GenIterable
  def indexDocuments(documentFiles: GenIterable[File]): Unit = {
    // index documents
    for (f <- documentFiles) {
      Try {
        val filename = StringField(name = "fileName", string = f.getName)
        val doc = {
          val d = Document.fromJson(f)
          d.copy(metadata = d.metadata ++ Seq(filename))
        }
        index.indexOdinsonDoc(doc)
      } match {
        case Success(_) =>
          logger.info(s"Indexed ${f.getName}")
        case Failure(e) =>
          logger.error(s"Failed to index ${f.getName}", e)
      }
    }
  }

}
