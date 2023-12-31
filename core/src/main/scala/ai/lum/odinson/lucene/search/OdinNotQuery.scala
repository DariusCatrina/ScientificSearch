package ai.lum.odinson.lucene.search

import java.util.{ Map => JMap, Set => JSet }
import org.apache.lucene.index._
import org.apache.lucene.search._
import org.apache.lucene.search.spans._
import ai.lum.odinson.lucene.search.spans._
import Spans._

class OdinNotQuery(
  val include: OdinsonQuery,
  val exclude: OdinsonQuery,
  val field: String
) extends OdinsonQuery { self =>

  override def hashCode: Int = (include, exclude, field).##

  def toString(field: String): String = {
    "NotQuery(" + include.toString(field) + "," + exclude.toString(field) + ")"
  }

  def getField(): String = field

  override def rewrite(reader: IndexReader): Query = {
    val rewrittenInclude = include.rewrite(reader).asInstanceOf[OdinsonQuery]
    val rewrittenExclude = exclude.rewrite(reader).asInstanceOf[OdinsonQuery]
    if (rewrittenInclude != include || rewrittenExclude != exclude) {
      new OdinNotQuery(rewrittenInclude, rewrittenExclude, field)
    } else {
      super.rewrite(reader)
    }
  }

  override def createWeight(
    searcher: IndexSearcher,
    needsScores: Boolean
  ): OdinsonWeight = {
    val includeWeight = include.createWeight(searcher, false).asInstanceOf[OdinsonWeight]
    val excludeWeight = exclude.createWeight(searcher, false).asInstanceOf[OdinsonWeight]
    val terms = OdinsonQuery.getTermContexts(includeWeight, excludeWeight)
    new OdinNotWeight(searcher, terms, includeWeight, excludeWeight)
  }

  class OdinNotWeight(
    searcher: IndexSearcher,
    terms: JMap[Term, TermContext],
    val includeWeight: OdinsonWeight,
    val excludeWeight: OdinsonWeight
  ) extends OdinsonWeight(self, searcher, terms) {

    def extractTerms(terms: JSet[Term]): Unit = {
      includeWeight.extractTerms(terms)
    }

    def extractTermContexts(contexts: JMap[Term, TermContext]): Unit = {
      includeWeight.extractTermContexts(contexts)
    }

    def getSpans(
      context: LeafReaderContext,
      requiredPostings: SpanWeight.Postings
    ): OdinsonSpans = {
      val includeSpans = includeWeight.getSpans(context, requiredPostings)
      if (includeSpans == null) return null
      val excludeSpans = excludeWeight.getSpans(context, requiredPostings)
      if (excludeSpans == null) return includeSpans
      val excludeTwoPhase = excludeSpans.asTwoPhaseIterator()
      val excludeApproximation =
        if (excludeTwoPhase == null) null else excludeTwoPhase.approximation()
      val spans = new FilterSpans(includeSpans) {
        import FilterSpans._
        // last document we have checked matches() against for the exclusion, and failed
        // when using approximations, so we don't call it again, and pass thru all inclusions.
        private var lastApproxDoc: Int = -1
        private var lastApproxResult: Boolean = false
        protected def accept(candidate: Spans): AcceptStatus = {
          val doc = candidate.docID()
          if (doc > excludeSpans.docID()) {
            // catch up 'exclude' to the current doc
            if (excludeTwoPhase != null) {
              if (excludeApproximation.advance(doc) == doc) {
                lastApproxDoc = doc
                lastApproxResult = excludeTwoPhase.matches()
              }
            } else {
              excludeSpans.advance(doc)
            }
          } else if (
            excludeTwoPhase != null && doc == excludeSpans.docID() && doc != lastApproxDoc
          ) {
            // excludeSpans already sitting on our candidate doc, but matches not called yet.
            lastApproxDoc = doc
            lastApproxResult = excludeTwoPhase.matches()
          }
          if (doc != excludeSpans.docID() || (doc == lastApproxDoc && !lastApproxResult)) {
            return AcceptStatus.YES
          }
          if (excludeSpans.startPosition() == -1) { // init exclude start position if needed
            excludeSpans.nextStartPosition()
          }
          val cmp =
            if (candidate.startPosition == candidate.endPosition) {
              // if candidate is empty then use less-than
              // this is a special case for zero-width assertions (lookarounds)
              (x: Int, y: Int) => x < y
            } else {
              // if candidate is not empty then use less-than-or-equal
              // because spans are inclusive at the start and exclusive at the end
              (x: Int, y: Int) => x <= y
            }
          while (cmp(excludeSpans.endPosition, candidate.startPosition)) {
            // exclude end position is before a possible exclusion
            if (excludeSpans.nextStartPosition() == NO_MORE_POSITIONS) {
              return AcceptStatus.YES // no more exclude at current doc.
            }
          }
          // exclude end position far enough in current doc, check start position:
          if (cmp(candidate.endPosition, excludeSpans.startPosition)) {
            AcceptStatus.YES
          } else {
            AcceptStatus.NO
          }
        }
      }
      new OdinSpansWrapper(spans)
    }

  }

}
