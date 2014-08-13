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
}