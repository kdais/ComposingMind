package Test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Core._

@RunWith(classOf[JUnitRunner])
class RegionSuite extends FunSuite {
  
  trait TestRegion {
    val dataLength = 10000
    val generator = new util.Random()
    def gen = generator.nextInt() % 2 != 0
    
    def genBoolVector(length : Int) = Vector.fill(length)(gen)
    
    val region = Region.genRegion(dataLength, 10, 5)
  }
  
  test ("Percentage of active columns is expected to be around 5%.") {
    new TestRegion {
      val data = genBoolVector(dataLength)
      val actives = region.activeColumns(data, 0.01F)
      println(actives)
      assert(actives.length >= 1 && actives.length <= 5)
    }
  }
}