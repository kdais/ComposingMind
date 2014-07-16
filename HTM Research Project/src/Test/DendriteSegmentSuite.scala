package Test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Core.ProximalSegment
import Core.DistalSegment

@RunWith(classOf[JUnitRunner])
class DendriteSegmentSuite extends FunSuite {
  
  trait TestSegments {
    val (length, boost, threshold, radius) = (10, 3, 0.5F, 0.3F)
    val indexes = List(1, 4, 6, 8, 12, 22, 25, 36, 40, 44)
    val permanences = ProximalSegment.genPermanences(length, threshold, radius)
    val myPermanences = List(0.6F, 0.3F, 0.5F, 0.5F, 0.3F, 0.8F, 0.6F, 0.4F, 0.6F, 0.9F)
    
    val data = Vector(1, 0, 0, 1, 1, 1, 0, 0, 1, 1,
    				  1, 0, 0, 1, 1, 1, 0, 0, 0, 1,
    				  1, 1, 1, 1, 0, 0, 0, 1, 0, 1,
    				  0, 0, 1, 0, 1, 1, 0, 0, 0, 0,
    				  1, 1, 0, 1, 0, 0, 1, 0, 0, 0)
  }
  
  test ("ProximalSegment : Random permanences within radius") {
    new TestSegments {
      assert(permanences.forall(p => p >= threshold - radius && p <= threshold + radius) === true)
      assert(permanences.exists(p => p != threshold) === true)
    }
  }
  
  test ("ProximalSegment : Overlap") {
    new TestSegments {
      val segment = new ProximalSegment(indexes.zip(myPermanences), threshold, boost)
      
      assert(segment.overlap(data, (x : Int) => x) === 9)
      assert(segment.withBoost(4).overlap(data, (x : Int) => x) === 12)
    }
  }
  
  test ("ProximalSegment : Updated permanences") {
    new TestSegments {
      val segment = new ProximalSegment(indexes.zip(myPermanences), threshold, boost)
      val updatedSegment = segment.updatePermanences(0.2F, data, (x : Int) => x)
      
      assert(updatedSegment.overlap(data, (x : Int) => x) === 12)
    }
  }
  
  test ("DistalSegment : Overlap") {
    new TestSegments {
      val segment = new DistalSegment(indexes, 3)
      
      assert(segment.overlap(data, (x : Int) => x) === 4)
    }
  }
  
  test ("DistalSegment : Expired") {
    new TestSegments {
      val segment = new DistalSegment(indexes, 2).updateExpirationTime().updateExpirationTime()
      
      assert(segment.expired === true)
    }
  }
}