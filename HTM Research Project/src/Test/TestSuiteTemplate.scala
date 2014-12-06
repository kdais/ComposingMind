package Test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTemplateSuite extends FunSuite {
  
  trait TestTemplate {
    val one = 1
    val two = 2
  }
  
  test ("1 + 2 == 3") {
    new TestTemplate {
      assert(one + two == 3 === true)
    }
  }
}