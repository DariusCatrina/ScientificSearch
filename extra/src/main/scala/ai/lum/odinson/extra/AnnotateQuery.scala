package ai.lum.odinson.extra


import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.{Document, Processor}
import com.typesafe.config.ConfigValueFactory
import ai.lum.common.ConfigUtils._
import org.clulab.struct.DirectedGraphEdgeIterator
import java.io._
import ai.lum.common.ConfigFactory
import ai.lum.odinson.extra.utils.ProcessorsUtils.getProcessor
object AnnotateQuery extends App {
var config = ConfigFactory.load()
val querysentence = config.apply[String]("odinson.extra.rawQuery")

// Create the processor.  Any processor works here!
// Try FastNLPProcessor or our own CluProcessor.
val processorType = config.apply[String]("odinson.extra.processorType")

// val proc: Processor = getProcessor(processorType)
val proc: Processor = new CoreNLPProcessor()

// The actual work is done here.
val doc: Document = proc.annotate(querysentence)

// You are basically done.  The rest of this code simply prints out the annotations.
val pw = new PrintWriter(new File("test.txt" ))

// Let's print the sentence-level annotations.
for ((sentence, sentenceIndex) <- doc.sentences.zipWithIndex) {
  println("Sentence #" + sentenceIndex + ":")
  println("Tokens: " + mkString(sentence.words))
  println("Start character offsets: " + mkString(sentence.startOffsets))
  println("End character offsets: " + mkString(sentence.endOffsets))

  // These annotations are optional, so they are stored using Option objects,
  // hence the foreach statement.
  sentence.lemmas.foreach(lemmas => println("Lemmas: " + mkString(lemmas)))
  sentence.tags.foreach(tags => println("POS tags: " + mkString(tags)))
  sentence.chunks.foreach(chunks => println("Chunks: " + mkString(chunks)))
  sentence.entities.foreach(entities => println("Named entities: " + mkString(entities)))
  sentence.norms.foreach(norms => println("Normalized entities: " + mkString(norms)))
  sentence.dependencies.foreach { dependencies =>
    println("Syntactic dependencies:")
    val iterator = new DirectedGraphEdgeIterator[String](dependencies)
    iterator.foreach { dep =>
      // Note that we use offsets starting at 0 unlike CoreNLP, which uses offsets starting at 1.
      println(" head: " + dep._1 + " modifier: " + dep._2 + " label: " + dep._3)
      pw.write(dep._1 + " " + dep._2 + " " + dep._3 +"\n")
    }
  }
  pw.close()
  sentence.syntacticTree.foreach { syntacticTree =>
    // See the org.clulab.utils.Tree class for more information
    // on syntactic trees, including access to head phrases/words.
    println("Constituent tree: " + syntacticTree)
  }
  println()
  println()
}

// Let's print the coreference chains.
doc.coreferenceChains.foreach { chains =>
  for (chain <- chains.getChains) {
    println("Found one coreference chain containing the following mentions:")
    for (mention <- chain) {
      val text = doc.sentences(mention.sentenceIndex).words
          .slice(mention.startOffset, mention.endOffset).mkString("[", " ", "]")
      // Note that all these offsets start at 0, too.
      println("\tsentenceIndex: " + mention.sentenceIndex +
          " headIndex: " + mention.headIndex +
          " startTokenOffset: " + mention.startOffset +
          " endTokenOffset: " + mention.endOffset +
          " text: " + text)
    }
  }
}


def mkString[T](elems: Array[T]): String = elems.mkString(" ")
}