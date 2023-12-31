package ai.lum.odinson.documentation

import ai.lum.odinson.Document
import ai.lum.odinson.test.utils.OdinsonTest

class TestDocumentationTokenConstraints extends OdinsonTest {

  def exampleSentence: String =
    """{"id":"dd","metadata":[],"sentences":[{"numTokens":5,"fields":[{"$type":"ai.lum.odinson.TokensField","name":"raw","tokens":["George","ate","gummy","bears","."],"store":true},{"$type":"ai.lum.odinson.TokensField","name":"word","tokens":["George","ate","gummy","bears","."]},{"$type":"ai.lum.odinson.TokensField","name":"tag","tokens":["NNP","VBD","JJ","NNS","."]},{"$type":"ai.lum.odinson.TokensField","name":"lemma","tokens":["george","eat","gummy","bear","."]},{"$type":"ai.lum.odinson.TokensField","name":"entity","tokens":["ORGANIZATION","O","O","O","O"]},{"$type":"ai.lum.odinson.TokensField","name":"chunk","tokens":["B-NP","I-NP","I-NP","I-NP","O"]},{"$type":"ai.lum.odinson.GraphField","name":"dependencies","edges":[[1,0,"nsubj"],[1,3,"dobj"],[1,4,"punct"],[3,2,"amod"]],"roots":[1]}]}]}"""

  "Documentation-TokenConstraints" should "work for 'Example'" in {
    val ee = mkExtractorEngineFromText("The dog barks")
    // what is there should match
    val q = ee.mkQuery("dog")
    val s = ee.query(q)
    numMatches(s) shouldEqual (1)
    // something that is not there should not match
    val q1 = ee.mkQuery("cat")
    val s1 = ee.query(q1)
    numMatches(s1) shouldEqual (0)
  }

  it should "work for 'Using the token fields'" in {
    val doc = Document.fromJson(exampleSentence)
    val ee = mkExtractorEngine(doc)
    // [tag=/N.*/]
    // get a document with tags
    val q = ee.mkQuery("[tag=/N.*/]")
    val s = ee.query(q)
    // 2 nouns
    numMatches(s) shouldEqual (2)
    //
    val q1 = ee.mkQuery("[tag=/V.*/]")
    val s1 = ee.query(q1)
    numMatches(s1) shouldEqual (1)
  }

  it should "work for 'Operators for token constraints'" in {
    val doc = Document.fromJson(exampleSentence)
    val ee = mkExtractorEngine(doc)
    // [tag=/N.*/ & (entity=ORGANIZATION | tag=NNP)]
    val q = ee.mkQuery("[tag=/N.*/ & (entity=ORGANIZATION | tag=NNP)]")
    val s = ee.query(q)
    numMatches(s) shouldEqual (1)
    // should not return
    val q1 = ee.mkQuery("[tag=/N.*/ & (entity=FOO | tag=BAR)]")
    val s1 = ee.query(q1)
    numMatches(s1) shouldEqual (0)
  }

  it should "work for 'Wildcards'" in {
    val doc = Document.fromJson(exampleSentence)
    val ee = mkExtractorEngine(doc)
    // testing wilcard
    val q = ee.mkQuery("[]")
    // make sure it compiles to the right thing
    q.toString shouldEqual ("AllNGramsQuery(1)")
    val s = ee.query(q)
    // each token in the sentence
    numMatches(s) shouldEqual (5)
  }

  it should "work for 'quantifiers'" in {
    val doc = Document.fromJson(exampleSentence)
    val ee = mkExtractorEngine(doc)
    // testing wilcard
    val q = ee.mkQuery("[chunk=B-NP] [chunk=I-NP]*")
    val s = ee.query(q)
    numMatches(s) shouldEqual (1)
    // make sure it extracts all 4 tokens
    existsMatchWithSpan(s, doc = 0, start = 0, end = 4)
  }
}
