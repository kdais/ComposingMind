package Test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Core.HuffmanCodec

@RunWith(classOf[JUnitRunner])
class HuffmanCodecSuite extends FunSuite {
  
  trait HuffmanCodecTest {
    val letters = List("A", "B", "C", "D", "E")
    val probabilities = List(0.4F, 0.15F, 0.15F, 0.15F, 0.15F)
    val alphabet : HuffmanCodec.Alphabet[String] = letters zip probabilities
    val codec = new HuffmanCodec(alphabet)
  }
  
  test ("Decoded code symbol") {
    new HuffmanCodecTest {
      assert(letters.forall(l => codec.decodeSymbol(codec.encodeSymbol(l))._1 == l))
    }
  }
  
  test ("Decoded code list of symbols") {
    new HuffmanCodecTest {
      assert(codec.decode(codec.encode(List("A", "B", "E", "A"))) === List("A", "B", "E", "A"))
    }
  }
}