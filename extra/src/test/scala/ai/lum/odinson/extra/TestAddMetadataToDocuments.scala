package ai.lum.odinson.extra

import java.io.{ File, IOException }
import java.nio.file.Files
import ai.lum.odinson.Document
import ai.lum.odinson.test.utils.OdinsonTest
import ai.lum.odinson.utils.exceptions.OdinsonException
import org.apache.commons.io.FileUtils

import scala.reflect.io.Directory

class TestAddMetadataToDocuments extends OdinsonTest {

  def directSettings(tmpDir: String): Map[String, Any] = {
    Map[String, Any](
      "metadata.originalDocsDir" -> s"${tmpDir}/docs",
      "metadata.metadataDir" -> s"${tmpDir}/metadata",
      "metadata.metadataExt" -> ".json",
      "metadata.finalDocsDir" -> s"${tmpDir}/docsWithMetadata",
      "metadata.append" -> false
    )
  }

  "AddMetadataToDocuments" should "read metadata from files and add to the documents" in {
    val resourcesFolder = getClass.getResource("/").getFile
    val tmpFolder = Files.createTempDirectory("odinson-test-resources").toFile

    val srcDir = new File(resourcesFolder)

    try {
      FileUtils.copyDirectory(srcDir, tmpFolder);
    } catch {
      case e: IOException =>
        throw OdinsonException("Can't copy resources directory")
    }

    // verify that the documents do not have metadata
    val docFiles = new File(s"${tmpFolder}/docs").listFiles()
    docFiles should have size (2)
    for (f <- docFiles) {
      val doc = Document.fromJson(f)
      doc.metadata.toArray shouldBe empty
    }

    // Add the metadata
    val settings = directSettings(tmpFolder.getAbsolutePath)
    AddMetadataToDocuments.usingConfigSettings(settings)

    // verify that the original documents still don't have metadata
    val docFiles2 = new File(s"${tmpFolder}/docs").listFiles()
    docFiles2 should have size (2)
    for (f <- docFiles2) {
      val doc = Document.fromJson(f)
      doc.metadata.toArray shouldBe empty
    }

    // verify that the new documents now have metadata and that they're in the right place
    val docFiles3 = new File(s"${tmpFolder}/docsWithMetadata").listFiles()
    docFiles3 should have size (2)
    for (f <- docFiles3) {
      val doc = Document.fromJson(f)
      doc.metadata.toArray shouldNot be(empty)
    }

    // delete the temp folder
    val dir = new Directory(tmpFolder)
    dir.deleteRecursively()
  }

}
