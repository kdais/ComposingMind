package Test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Core.ProximalSegment

@RunWith(classOf[JUnitRunner])
class DendriteSegmentSuite extends FunSuite {
  
  trait TestSegments {
    val (length, boost, threshold, radius) = (10, 3, 0.5F, 0.3F)
    val indexes = List(1, 4, 6, 8, 12, 22, 25, 36, 40, 44)
    val permanences = ProximalSegment.genPermanences(length, threshold, radius)
    val segment = new ProximalSegment(indexes.zip(permanences), threshold, boost)
  }
  
  test ("DendriteSegment : Random permanences within radius") {
    new TestSegments {
      assert(permanences.forall(p => p >= threshold - radius && p <= threshold + radius) === true)
      assert(permanences.exists(p => p != threshold) === true)
    }
  }
}