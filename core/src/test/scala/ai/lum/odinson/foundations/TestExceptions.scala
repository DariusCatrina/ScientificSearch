package ai.lum.odinson.foundations

import ai.lum.odinson.test.utils.OdinsonTest
import ai.lum.odinson.utils.exceptions.OdinsonException

class TestExceptions extends OdinsonTest {

  "OdinsonException" should "properly throw exceptions" in {

    def exceptionThrower(bool: Boolean) = {
      bool match {
        case true  => throw OdinsonException("we threw an odinson exception!")
        case false => ()
      }
    }

    noException should be thrownBy exceptionThrower(false)
    an[OdinsonException] should be thrownBy exceptionThrower(true)
    an[Exception] should be thrownBy exceptionThrower(true)
  }

}
