package ai.lum.odinson.extra

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
import ai.lum.odinson.lucene.search.highlight.ConsoleHighlighter

object ScientificSearchEngine extends App with LazyLogging {

  println("Warming up Engine, please wait...")
  // raw scala service designed to interact with py code
  // Specify paths and settings in the local config file
  val debug = false
  val print_query=true
  val config = ConfigFactory.load()
  val outputFile = config.apply[File]("odinson.extra.outputFile")
  val rulesFile = config.apply[String]("odinson.extra.rulesFile")
  val displayField = config.apply[String]("odinson.displayField")

  // Initialize the extractor engine, using the index specified in the config

  val extractorEngine = ExtractorEngine.fromConfig()
  // val proc: Processor = new FastNLPProcessor()
  val processorType = config.apply[String]("odinson.extra.processorType")
  val proc: Processor = getProcessor(processorType)
  proc.annotate("Warm up")

  println("Initialization Complete. Ready for execution.")

  try {
    // run the shell
    var running = true
    while (running) {
      try {
        println("Enter command:")
        val line = readLine()
        if (line == null) {
          println(":exit")
          running = false
        } else {
          line.trim match {
            case ":search" =>
              println("Enter Query String:")
              val querySentence: String = readLine()
              val doc: Document = proc.annotate(querySentence)
              val words = ArrayBuffer[String]();
              var idx: Int = 0;
              val queryCaptures = ArrayBuffer[Int]()
              val anchors = ArrayBuffer[Int]()
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
              if (debug) {
                println("Raw sentence ")
                println(words.mkString(" "))
              }
              val (rawToken, rawEdgeLabel, rawGraph) = convertSentenceToGraph(words.mkString(" "))
              if (debug) {
                print("Raw Graph: ")
                println(rawGraph)
              }
              val (queryGraph, queryEdges) =
                buildQueryGraph(anchors, queryCaptures, rawEdgeLabel, rawGraph)
              val longestPath = getLongestPath(queryGraph)
              val queryString =
                getQueryString(longestPath, queryEdges, anchors, queryCaptures, rawToken)
              if (print_query) {
                print("Query String: ")
                println(queryString)
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
              println("Printing Matches: ")
              var matchID: Int = 1
              for (hit <- results.scoreDocs) {
                val doc = extractorEngine.doc(hit.doc)
                val docID = doc.getField("docId").stringValue
                val spans = hit.matches.toVector
                val captures = hit.matches.flatMap(_.namedCaptures).toVector
                val resultString = extractorEngine.index.doc(hit.doc).getField("raw").stringValue
                val (matchedMapping, isValid) = validateResult(
                  resultString,
                  captures,
                  queryGraph,
                  queryEdges,
                  queryCaptures,
                  anchors,
                  rawToken
                )
                if (isValid) {
                  println("Match #" + matchID + ":")
                  matchID += 1
                  println(s"Doc $docID (lucene doc = ${hit.doc}   score = ${hit.score})")
                  var newCaptures = Vector[NamedCapture]()
                  for ((queryId, destId) <- matchedMapping) {
                    val capturedMatch = StateMatch(destId, destId + 1, Array[NamedCapture]())
                    if (anchors.contains(queryId)) {
                      newCaptures = newCaptures :+ NamedCapture(
                        rawToken(queryId),
                        label = Option[String]("anchor"),
                        capturedMatch
                      )
                    } else if (queryCaptures.contains(queryId)) {
                      newCaptures = newCaptures :+ NamedCapture(
                        rawToken(queryId),
                        None,
                        capturedMatch
                      )
                    }
                  }
                  val res = ConsoleHighlighter.highlight(
                    index = extractorEngine.index,
                    docId = hit.doc,
                    field = displayField,
                    spans = spans,
                    captures = newCaptures
                  )

                  println(res)
                  println()
                }
              }
              println("All Matches Printed!")
            case ":exit" => running = false
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

  // function to annotate sentence and convert to graph
  def convertSentenceToGraph(sent: String)
    : (HashMap[Int, String], HashMap[(Int, Int), String], HashMap[Int, HashSet[Int]]) = {
    val doc: Document = proc.annotate(sent)
    // maps idx to token
    var idx2token = new HashMap[Int, String]()
    // maps edges to edge label
    var edge2label = new HashMap[(Int, Int), String]()
    // undirected (bi-directional) graph
    var graph = new HashMap[Int, HashSet[Int]]()

    // record token id
    for ((token, idx) <- sent.split(" ").zipWithIndex) {
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

  // reduce graph to only connections between anchors captures
  def buildQueryGraph(
    anchors: ArrayBuffer[Int],
    captures: ArrayBuffer[Int],
    rawEdgeLabel: HashMap[(Int, Int), String],
    rawGraph: HashMap[Int, HashSet[Int]]
  ): (HashMap[Int, HashSet[Int]], HashMap[(Int, Int), String]) = {
    var queryGraph = new HashMap[Int, HashSet[Int]]()
    var queryEdges = new HashMap[(Int, Int), String]()
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
    for (node1 <- anchors ++ captures) {
      for (node2 <- anchors ++ captures) {
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
    return (queryGraph, queryEdges)
  }

  // function thawt gets query sentence
  def getLongestPath(queryGraph: HashMap[Int, HashSet[Int]]): ArrayBuffer[Int] = {
    var longestPath = ArrayBuffer[Int]()
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
    return longestPath
  }

  // function that converts path to a readable query
  def getQueryString(
    path: ArrayBuffer[Int],
    queryEdges: HashMap[(Int, Int), String],
    anchors: ArrayBuffer[Int],
    captures: ArrayBuffer[Int],
    tokens: HashMap[Int, String]
  ): String = {
    val queryStringBuilder = ArrayBuffer[String]()
    for (i <- 0 to path.length - 2) {
      val cur = path(i)
      val next = path(i + 1)
      if (anchors.contains(cur)) {
        if (tokens(cur).charAt(tokens(cur).length - 1) != '.') {
          queryStringBuilder.append("(?<node" + cur + ">[norm=" + tokens(cur) + "])")
        } else {
          queryStringBuilder.append("(?<node" + cur + ">[norm=" + tokens(cur).substring(
            0,
            tokens(cur).length - 1
          ) + "])")
        }
      } else {
        queryStringBuilder.append("(?<node" + cur + ">[])")
      }
      queryStringBuilder.append(queryEdges((cur, next)))
    }
    if (path.length != 0) {
      val last = path(path.length - 1)
      if (anchors.contains(last)) {
        queryStringBuilder.append("(?<node" + last + ">[norm=" + tokens(last) + "])")
      } else {
        queryStringBuilder.append("(?<node" + last + ">[])")
      }
    }
    val queryStringRaw = queryStringBuilder.mkString(" ")
    return queryStringRaw
  }

  /** validates if a match satisfies constraints */
  def validateResult(
    matchSentence: String,
    resCaptures: Vector[NamedCapture],
    queryGraph: HashMap[Int, HashSet[Int]],
    queryEdges: HashMap[(Int, Int), String],
    queryCaptures: ArrayBuffer[Int],
    anchors: ArrayBuffer[Int],
    tokens: HashMap[Int, String]
  ): (HashMap[Int, Int], Boolean) = {
    // convert matched sentence to graph
    val (resTokens, resEdges, resGraph) = convertSentenceToGraph(matchSentence)
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
            breakable{
              if ((resStart == matchedMapping(start)) && label == resLabel) {
                // if is anchor, check if match the exact word
                if (anchors.contains(end) && resTokens(resEnd).toLowerCase() != tokens(end).toLowerCase()){
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
    return completeMatching(unmatchedEdges, matchedMapping)

  }

}
