package Core

/**
 * @brief class for encoding changes in input data.
 * @param value input value.
 * @param diff function for calculating difference between current and next value.
 * @param codec binary codec for encoding difference.
 */
class DeltaValue(val value : String, val diff : (String, String) => String, val codec : BinaryCodec[String]) {
  
  private def delta(newValue : String) : Vector[Byte] =
    codec.encodeSymbol(diff(value, newValue))
    
  private def reverseDelta(code : Vector[Byte]) : (String, Vector[Byte]) = {
    val (delta, codeTail) = codec.decodeSymbol(code)
    (diff(value, delta), codeTail)
  }
    
  private def update(newValue : String) = new DeltaValue(newValue, diff, codec)
}

/**
 * @brief DeltaValue object.
 */
object DeltaValue {
  
  /**
   * Encodes delta between current and next value.
   * @param value next value.
   * @return state with encoded delta and new DeltaValue. 
   */
  def doDelta(value : String): State[DeltaValue, Vector[Byte]] =
    new State((d : DeltaValue) => (d.delta(value), State.lazyState(d.update(value))))
  
  /**
   * Decodes delta from binary code and current value.
   * @param code binary code.
   * @return state with decoded delta, rest of code and new DeltaValue. 
   */
  def doReverseDelta(code : Vector[Byte]): State[DeltaValue, (String, Vector[Byte])] =
    new State((d : DeltaValue) => {
      val (value, codeTail) = d.reverseDelta(code)
      ((value, codeTail), State.lazyState(d.update(value)))
    })
}