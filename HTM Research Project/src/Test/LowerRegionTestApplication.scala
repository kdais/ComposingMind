package Test

import Core._

object LowerRegionTestApplication extends App {
  def intsToDeltas(ints : List[Int]) = List.range(ints.min - ints.max, ints.max - ints.min + 1, 1)
  
  def intsToString(ints : List[Int]) = ints.map(_.toString)
  
  override def main(args : Array[String]) : Unit = {
    val numbers = List.range(0, 100, 1)
    val deltas = intsToDeltas(numbers)
    val alphabet = intsToString(deltas) zip List.fill(deltas.length)(1.0F / deltas.length)
    val codec = new HuffmanCodec(alphabet)
    
    println(codec.encode(intsToString(List(99))))
  }
}