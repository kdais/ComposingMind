package Core

/**
 * @brief class for encoding changes in input data.
 * @param value input value.
 * @param diff function for calculating difference between current and next value.
 * @param codec binary codec for encoding difference.
 */
class DeltaValue[T](val value : T, val diff : (T, T) => T, val codec : BinaryCodec[T]) {
  
  private def delta(newValue : T) : Vector[Byte] =
    codec.encodeSymbol(diff(value, newValue))
    
  private def reverseDelta(code : Vector[Byte]) : (T, Vector[Byte]) = {
    val (delta, codeTail) = codec.decodeSymbol(code)
    (diff(delta, value), codeTail)
  }
    
  private def update(newValue : T) = new DeltaValue(newValue, diff, codec)
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
  def doDelta[T](value : T): State[DeltaValue[T], Vector[Byte]] =
    new State((d : DeltaValue[T]) => (d.delta(value), State.lazyState(d.update(value))))
  
  /**
   * Decodes delta from binary code and current value.
   * @param code binary code.
   * @return state with decoded delta, rest of code and new DeltaValue. 
   */
  def doReverseDelta[T](code : Vector[Byte]): State[DeltaValue[T], (T, Vector[Byte])] =
    new State((d : DeltaValue[T]) => {
      val (value, codeTail) = d.reverseDelta(code)
      ((value, codeTail), State.lazyState(d.update(value)))
    })
}