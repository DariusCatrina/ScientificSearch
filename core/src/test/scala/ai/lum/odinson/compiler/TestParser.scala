package ai.lum.odinson.compiler

import ai.lum.odinson.test.utils.OdinsonTest

class TestParser extends OdinsonTest {

  val parser = new QueryParser(Seq("norm", "word", "tag"), "norm")

  "Literals" should "handle identifiers that start with underscore" in {
    noException should be thrownBy parser.parseBasicQuery("_")
    noException should be thrownBy parser.parseBasicQuery("_test")
  }

  it should "handle extended identifiers that start with underscore" in {
    noException should be thrownBy parser.parseBasicQuery("[word=_]")
    noException should be thrownBy parser.parseBasicQuery("[word=_test]")
    noException should be thrownBy parser.parseBasicQuery("a >_ b")
    noException should be thrownBy parser.parseBasicQuery("a >_test b")
  }

}
