package engine

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
import java.sql.{ Connection, DriverManager, ResultSet }

import ai.lum.odinson._
import ai.lum.odinson.lucene.search._
import ai.lum.common.DisplayUtils._
import ai.lum.odinson.lucene._
import ai.lum.odinson.lucene.search.highlight.HtmlHighlighter
import org.apache.lucene.document.{ Document => LuceneDocument }
import ai.lum.odinson.{ Document => OdinsonDocument }
import ai.lum.odinson.GraphField

import scala.collection.JavaConversions._

object QueryExceptions {
  class EmptyQueryException extends RuntimeException
  class InvalidQueryException extends RuntimeException
  class SearchNotReadyException extends RuntimeException
}

class Query(val querySentence: String, var debug: Boolean = false, var printQuery: Boolean = true) {
  print(querySentence)
  var words = ArrayBuffer[String]();
  var queryCaptures = ArrayBuffer[Int]()
  var anchors = ArrayBuffer[Int]()
  var rawToken = HashMap[Int, String]()
  var rawEdgeLabel = HashMap[(Int, Int), String]()
  var rawGraph = HashMap[Int, HashSet[Int]]()
  var queryGraph = HashMap[Int, HashSet[Int]]()
  var queryEdges = HashMap[(Int, Int), String]()
  var longestPath = ArrayBuffer[Int]()
  var queryString: String = ""
  var searchReady = false
  var scoreDocs = Array[OdinsonScoreDoc]()
  var curID: Int = 0

  val getContext = false

  var config = ConfigFactory.load()

  val docsDir = config.apply[File]("odinson.docsDir")

  var idx: Int = 0

  for (word <- querySentence.split(" ")) {
    if (word.charAt(0) == ':') {
      queryCaptures += idx
      words += word.substring(1)
    } else if (word.charAt(0) == '$') {
      anchors += idx
      words += word.substring(1)
    } else {
      words += word
    }
    idx += 1
  }
  if (anchors.length + queryCaptures.length == 0) {
    throw new QueryExceptions.EmptyQueryException
  }
  if (anchors.length + queryCaptures.length == 1) {
    if (queryCaptures.length == 1) {
      throw new QueryExceptions.InvalidQueryException
    }
  }

  /** wrapper function for all query pre-processing * */
  def preProcess(proc: Processor) {
    getRawGraph(proc)
    buildQueryGraph(proc)
    getLongestPath()
    getQueryString()
    searchReady = true
  }

  /** search and generate results * */
  def search(proc: Processor, extractorEngine: ExtractorEngine) = {
    if (!searchReady) {
      throw new QueryExceptions.SearchNotReadyException
    }
    var after: OdinsonScoreDoc = null
    val start = System.currentTimeMillis()
    val q = extractorEngine.compiler.mkQuery(queryString)
    val results = extractorEngine.query(q)
    val duration = (System.currentTimeMillis() - start) / 1000f
    after = results.scoreDocs.lastOption.getOrElse(null)
    val totalHits = results.totalHits

    val end = start + results.scoreDocs.length - 1
    val filtered_result = ArrayBuffer[OdinsonScoreDoc]()
    var matchID: Int = 1
    var curID: Int = 1
    scoreDocs = results.scoreDocs
  }

  def getDocTitles(docIDs: ListBuffer[String]): ListBuffer[String] = {
    classOf[org.postgresql.Driver]
    val con_str = "jdbc:postgresql://localhost:5432/postgres?user=postgres&password=gui0721"
    // val conn = DriverManager.getConnection(con_str)
    val resTitles = ListBuffer[String]()
    for (docID <- docIDs) {
      // try {
      //   val stm = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)
      //   val rs = stm.executeQuery("SELECT * from pubmed where id = " + docID)
      //   if (rs.next) {
      //     resTitles += rs.getString("title")
      //   } else {
      //     resTitles += docID
      //   }
      // } catch {
      resTitles += docID
      // }
    }
    // conn.close()
    assert(docIDs.length == resTitles.length)
    return resTitles
  }

  /** get next numToDisplay results * */
  def generateResult(
    numToDisplay: Int,
    extractorEngine: ExtractorEngine,
    proc: Processor,
    displayField: String
  ): (
    ListBuffer[String],
    ListBuffer[String],
    ListBuffer[String],
    HashMap[(String, String), Int],
    ListBuffer[(String, String)]
  ) = {
    val resultText = ListBuffer[String]()
    val resultCaptures = ListBuffer[(String, String)]()
    val resultDoc = ListBuffer[String]()
    val captureCounts = HashMap[(String, String), Int]()
    var matchID: Int = 1

    // template to store count
    val capturesBuilder = ListBuffer[String]()
    val capturesIds = ListBuffer[String]()
    for (i <- queryCaptures) {
      capturesBuilder.append(rawToken(i))
      capturesIds.append(i.toString)
    }
    val capturesString = capturesBuilder.mkString("$")
    while (curID < scoreDocs.length) {
      val hit = scoreDocs(curID)
      val doc = extractorEngine.doc(hit.doc)
      val docID = doc.getField("docId").stringValue
      val spans = hit.matches.toVector
      val captures = hit.matches.flatMap(_.namedCaptures).toVector
      val resultString =
        extractorEngine.index.doc(hit.doc).getField("raw").stringValue
      val resultTokens = resultString.split(" ")

      var newCaptures = ListBuffer[NamedCapture]()
      if (longestPath.length == queryGraph.size) {
        if (matchID % numToDisplay == 1 & matchID > 1 & numToDisplay != -1) {
          return (resultText, resultDoc, getDocTitles(resultDoc), captureCounts, resultCaptures)
        }
        matchID += 1
        // filter out those not in capture
        for (c <- captures) {
          val nodeID = c.name.substring(c.name.length - 1).toInt
          if (anchors.contains(nodeID)) {
            newCaptures = newCaptures :+ NamedCapture(
              nodeID.toString,
              Option[String]("anchor"),
              c.capturedMatch
            )
          } else if (queryCaptures.contains(nodeID)) {
            newCaptures = newCaptures :+ NamedCapture(
              nodeID.toString,
              None,
              c.capturedMatch
            )
          }
        }
      } else {
        val (matchedMapping, isValid) = validateResult(
          proc,
          extractorEngine.index.doc(hit.doc),
          resultString,
          captures
        )
        if (isValid || anchors.length + queryCaptures.length == 1) {
          print("Algorithm")
          print(matchID)
          if (matchID % numToDisplay == 1 & matchID > 1 & numToDisplay != -1) {
            print("hERE")
            return (resultText, resultDoc, getDocTitles(resultDoc), captureCounts, resultCaptures)
          }
          matchID += 1

          for ((queryId, destId) <- matchedMapping) {
            val capturedMatch = StateMatch(destId, destId + 1, Array[NamedCapture]())
            if (anchors.contains(queryId)) {
              newCaptures = newCaptures :+ NamedCapture(
                queryId.toString,
                label = Option[String]("anchor"),
                capturedMatch
              )
            } else if (queryCaptures.contains(queryId)) {
              newCaptures = newCaptures :+ NamedCapture(
                queryId.toString,
                None,
                capturedMatch
              )
            }
          }
        }
      }
      val chunkedCapture = ListBuffer[NamedCapture]()
      // add parameter here to enable / disable chunking
      if (true) {
        for (cap <- newCaptures) {
          if (cap.label == None) {
            chunkedCapture += getChunk(extractorEngine.index.doc(hit.doc), cap)
          } else {
            chunkedCapture += cap
          }
        }
      }
      // display results
      val res = HtmlHighlighter.highlight(
        index = extractorEngine.index,
        docId = hit.doc,
        field = displayField,
        spans = spans,
        captures = chunkedCapture
      )

      var prevText: String = ""
      if (getContext == true) {
        try {
          prevText = extractorEngine.index.doc(hit.doc - 1).getField("raw").stringValue
        } catch {
          case e => prevText = ""
        }
        var nextText: String = ""
        try {
          nextText = extractorEngine.index.doc(hit.doc + 1).getField("raw").stringValue
        } catch {
          case e => nextText = ""
        }

        resultText += "..." + prevText + " " + res + " " + nextText + "..."
        resultDoc += docID
      } else {
        resultText += res
        resultDoc += docID
      }
      // add to the count
      val capWordsBuilder = ListBuffer[String]()
      var finish = true
      for (_ <- capturesIds) {
        capWordsBuilder.append("")
      }
      for (c <- chunkedCapture) {
        // we only care about captures
        if (c.label == None) {
          val id = c.name

          // build single capture word
          val singleCapWordBuilder = ArrayBuffer[String]()
          for (i <- c.capturedMatch.start until c.capturedMatch.end) {
            singleCapWordBuilder.append(resultTokens(i))
          }
          val singleCapWord = singleCapWordBuilder.mkString(" ")

          // add to list of capture
          for ((capId, i) <- capturesIds.view.zipWithIndex) {
            if (id == capId) {
              capWordsBuilder(i) = singleCapWord
            }
          }
          finish = true
          // check if finish
          for (w <- capWordsBuilder) {
            if (w == "") {
              finish = false
            }
          }
          // if finish, add to count
          if (finish) {
            val capWordsString = capWordsBuilder.mkString("$")
            val itemToAdd = (capturesString, capWordsString)
            // re-initialize
            for (i <- 0 until capturesIds.length) {
              capWordsBuilder(i) = ""
            }
            // add to count
            if (captureCounts.contains(itemToAdd)) {
              captureCounts(itemToAdd) += 1
            } else {
              captureCounts(itemToAdd) = 1
            }
            resultCaptures += itemToAdd
          }

        }
      }
      curID += 1
    }
    return (resultText, resultDoc, getDocTitles(resultDoc), captureCounts, resultCaptures)
  }

  /** convert the match to the chunk it belongs to * */
  def getChunk(doc: LuceneDocument, cap: NamedCapture): NamedCapture = {
    // get document
    // convert graph to a uunderstandable form
    val docId = doc.getField("docId").stringValue
    val sentId = doc.getField("sentId").stringValue.toInt
    val documentFile = new File(docsDir, docId + ".json.gz")
    val documentOdin = OdinsonDocument.fromJson(documentFile)
    val odinGraphs = documentOdin.sentences(sentId).fields.collect { case g: TokensField => g }
    // ASSUME we have single word in result
    val chunks = odinGraphs(5).tokens
    val startChunk = chunks(cap.capturedMatch.start)
    // find start and end chunk
    val startn = cap.capturedMatch.start
    val endn = cap.capturedMatch.end
    var start = startn
    var end = endn
    if (startChunk != "O") {

      // find start of chunk
      while (chunks(start).split("-")(0) != "B") {
        start -= 1
      }
      // find end of chunk
      while (end < chunks.length && chunks(end).split("-")(0) != "B" && chunks(end) != "O") {
        end += 1
      }
    }
    return NamedCapture(cap.name, cap.label, StateMatch(start, end, Array[NamedCapture]()))

  }

  /** helper function to validate result * */
  def validateResult(
    proc: Processor,
    matchDoc: LuceneDocument,
    matchSentence: String,
    resCaptures: Vector[NamedCapture]
  ): (HashMap[Int, Int], Boolean) = {
    // convert matched sentence to graph
    var start = System.currentTimeMillis()
    val (resTokens, resEdges, resGraph) =
      convertSentenceForValidation(matchSentence, matchDoc)
    var duration = (System.currentTimeMillis() - start) / 1000f
    print("Time for conversion: ")
    println(duration)
    start = System.currentTimeMillis()
    // build mapping: maps from query graph nodes to matched nodes
    val matchedMapping = new HashMap[Int, Int]()
    for (cap <- resCaptures) {
      matchedMapping(cap.name.substring(4).toInt) = cap.capturedMatch.start
    }
    // println(matchedMapping)

    var unmatchedEdges = queryEdges.clone()
    // println(unmatchedEdges)
    unmatchedEdges = unmatchedEdges.filter(x =>
      !(matchedMapping.contains(x._1._1) && matchedMapping.contains(x._1._2))
    )
    // println(unmatchedEdges)

    // helper function to fulfill a mapping. Returns false if a mapping is invalid else true
    def completeMatching(
      unmatchedEdges: HashMap[(Int, Int), String],
      matchedMapping: HashMap[Int, Int]
    ): (HashMap[Int, Int], Boolean) = {
      // base case
      if (unmatchedEdges.isEmpty) {
        return (matchedMapping, true)
      }
      // check any match
      for (((start, end), label) <- unmatchedEdges) {
        // already checked the reverse edge
        if (matchedMapping.contains(start) && matchedMapping.contains(end)) {
          val nextUnmatchedEdges = unmatchedEdges.clone()
          nextUnmatchedEdges.remove((start, end))
          val nextMatchedMapping = matchedMapping.clone()
          val (newMapping, found) =
            completeMatching(nextUnmatchedEdges, nextMatchedMapping)
          if (found) {
            return (newMapping, true)
          }
        }
        if (matchedMapping.contains(start)) {
          // find all edges that match the current edge
          for (((resStart, resEnd), resLabel) <- resEdges) {
            // found a match
            breakable {
              if ((resStart == matchedMapping(start)) && label == resLabel) {
                // if is anchor, check if match the exact word
                if (
                  anchors.contains(end) && resTokens(resEnd).toLowerCase() != rawToken(
                    end
                  ).toLowerCase()
                ) {
                  break
                }

                // match and recurse
                val nextUnmatchedEdges = unmatchedEdges.clone()
                nextUnmatchedEdges.remove((start, end))
                val nextMatchedMapping = matchedMapping.clone()
                nextMatchedMapping(end) = resEnd
                val (newMapping, found) = completeMatching(
                  nextUnmatchedEdges,
                  nextMatchedMapping
                )
                if (found) {
                  return (newMapping, true)
                }
              }
            }
          }
        }
      }
      return (HashMap[Int, Int](), false)
    }
    val res = completeMatching(unmatchedEdges, matchedMapping)
    duration = (System.currentTimeMillis() - start) / 1000f
    print("Time for subgraph matching: ")
    println(duration)
    return res

  }

  /** helper function * */
  def convertSentenceToGraph(
    proc: Processor,
    querySentence: String
  ): (HashMap[Int, String], HashMap[(Int, Int), String], HashMap[Int, HashSet[Int]]) = {
    val doc: Document = proc.annotate(querySentence)
    // maps idx to token
    var idx2token = new HashMap[Int, String]()
    // maps edges to edge label
    var edge2label = new HashMap[(Int, Int), String]()
    // undirected (bi-directional) graph
    var graph = new HashMap[Int, HashSet[Int]]()

    // record token id
    for ((token, idx) <- querySentence.split(" ").zipWithIndex) {
      idx2token += (idx -> token)
    }
    // convert graph to a uunderstandable form
    for ((sentence, sentenceIndex) <- doc.sentences.zipWithIndex) {
      sentence.dependencies.foreach { dependencies =>
        val iterator = new DirectedGraphEdgeIterator[String](dependencies)
        iterator.foreach { dep =>
          // Note that we use offsets starting at 0 unlike CoreNLP, which uses offsets starting at 1.
          if (debug) {
            println(dep._1 + " " + dep._2 + " " + dep._3)
          }

          // Process
          if (!graph.contains(dep._1)) {
            graph(dep._1) = new HashSet[Int]()
          }
          if (!graph.contains(dep._2)) {
            graph(dep._2) = new HashSet[Int]()
          }
          graph(dep._1) += dep._2
          graph(dep._2) += dep._1
          edge2label((dep._1, dep._2)) = '>' + dep._3
          edge2label((dep._2, dep._1)) = '<' + dep._3
        }
      }
    }
    return (idx2token, edge2label, graph)
  }

  /** helper function * */
  def convertSentenceForValidation(
    querySentence: String,
    doc: LuceneDocument
  ): (HashMap[Int, String], HashMap[(Int, Int), String], HashMap[Int, HashSet[Int]]) = {
    // maps idx to token
    var idx2token = new HashMap[Int, String]()
    // maps edges to edge label
    var edge2label = new HashMap[(Int, Int), String]()
    // undirected (bi-directional) graph
    var graph = new HashMap[Int, HashSet[Int]]()

    // record token id
    for ((token, idx) <- querySentence.split(" ").zipWithIndex) {
      idx2token += (idx -> token)
    }
    // convert graph to a uunderstandable form
    val docId = doc.getField("docId").stringValue
    val sentId = doc.getField("sentId").stringValue.toInt

    val documentFile = new File(docsDir, docId + ".json.gz")
    val documentOdin = OdinsonDocument.fromJson(documentFile)

    val odinGraphs = documentOdin.sentences(sentId).fields.collect { case g: GraphField => g }
    for ((src, dst, label) <- odinGraphs(0).edges) {
      if (!graph.contains(src)) {
        graph(src) = new HashSet[Int]()
      }
      if (!graph.contains(dst)) {
        graph(dst) = new HashSet[Int]()
      }
      graph(src) += dst
      graph(dst) += src
      edge2label((src, dst)) = '>' + label
      edge2label((dst, src)) = '<' + label
    }
    return (idx2token, edge2label, graph)
  }

  /** Process: convert sentence to graph * */
  def getRawGraph(proc: Processor) = {
    val res = convertSentenceToGraph(proc, words.mkString(" "))
    rawToken = res._1
    rawEdgeLabel = res._2
    rawGraph = res._3
    if (debug) {
      print("Raw Graph: ")
      println(rawGraph)
    }
  }

  /** Process: build query graph * */
  def buildQueryGraph(proc: Processor) {
    def bfs(start: Int, end: Int): Unit = {
      // BFS to find path between start and end and keep the edges within the path
      if (start == end) {
        return
      }
      val q = Queue[Int](start)
      val visited = HashSet[Int](start)
      var found = false
      val parent = HashMap[Int, Int]()
      while (!q.isEmpty && !found) {
        var cur = q.dequeue()
        if (cur == end) {
          while (cur != start) {
            queryEdges((cur, parent(cur))) = rawEdgeLabel((cur, parent(cur)))
            queryEdges((parent(cur), cur)) = rawEdgeLabel((parent(cur), cur))
            cur = parent(cur)
          }
          found = true
        }
        if (!found) {
          for (nextNode <- rawGraph(cur)) {
            if (!visited.contains(nextNode)) {
              parent(nextNode) = cur
              q.enqueue(nextNode)
              visited += nextNode
            }
          }
        }
      }
    }
    for (node1 <- anchors ++ queryCaptures) {
      for (node2 <- anchors ++ queryCaptures) {
        bfs(node1, node2)
      }
    }
    for (((i, j), k) <- queryEdges) {
      if (!queryGraph.contains(i)) {
        queryGraph(i) = new HashSet[Int]()
      }
      if (!queryGraph.contains(j)) {
        queryGraph(j) = new HashSet[Int]()
      }
      queryGraph(i) += j
      queryGraph(j) += i
    }
  }

  /** Process: getLongestPath * */
  def getLongestPath() = {

    def getPath(cur: Int, parent: HashMap[Int, Int]): ArrayBuffer[Int] = {
      var cu: Int = cur
      val res = ArrayBuffer[Int](cu)
      while (parent.contains(cu)) {
        cu = parent(cu)
        res.append(cu)
      }
      return res.reverse
    }
    for ((node, _) <- queryGraph) {
      val visited = HashSet[Int](node)
      var found = false
      val parent = HashMap[Int, Int]()
      var stack = Stack[(Int, Int)]((node, 1))
      while (!stack.isEmpty) {
        var (cur, length) = stack.pop
        if (length > longestPath.length) {
          longestPath = getPath(cur, parent)
        }
        for (nextNode <- queryGraph(cur)) {
          if (!visited.contains(nextNode)) {
            parent(nextNode) = cur
            stack.push((nextNode, length + 1))
            visited += nextNode
          }
        }
      }
    }
  }

  // Process: converts path to a readable query
  def getQueryString(
  ): Unit = {
    if (anchors.length + queryCaptures.length == 1) {
      val cur = anchors(0)
      if (rawToken(cur).charAt(rawToken(cur).length - 1) != '.') {
        queryString = "(?<node" + cur + ">[norm=" + rawToken(cur) + "])"
        return ()
      } else {
        queryString = "(?<node" + cur + ">[norm=" + rawToken(cur).substring(
          0,
          rawToken(cur).length - 1
        ) + "])"
        return ()
      }
    }
    val queryStringBuilder = ArrayBuffer[String]()
    for (i <- 0 to longestPath.length - 2) {
      val cur = longestPath(i)
      val next = longestPath(i + 1)
      if (anchors.contains(cur)) {
        if (rawToken(cur).charAt(rawToken(cur).length - 1) != '.') {
          queryStringBuilder.append("(?<node" + cur + ">[norm=" + rawToken(cur) + "])")
        } else {
          queryStringBuilder.append("(?<node" + cur + ">[norm=" + rawToken(cur).substring(
            0,
            rawToken(cur).length - 1
          ) + "])")
        }
      } else {
        queryStringBuilder.append("(?<node" + cur + ">[])")
      }
      queryStringBuilder.append(queryEdges((cur, next)))
    }
    if (longestPath.length != 0) {
      val last = longestPath(longestPath.length - 1)
      if (anchors.contains(last)) {
        queryStringBuilder.append("(?<node" + last + ">[norm=" + rawToken(last) + "])")
      } else {
        queryStringBuilder.append("(?<node" + last + ">[])")
      }
    }
    queryString = queryStringBuilder.mkString(" ")
    if (printQuery) {
      print("Query String: ")
      println(queryString)
    }
  }

}
