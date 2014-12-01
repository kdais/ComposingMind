package Core

/**
 * @brief class for handling list of delta values.
 * @param 
 */
class DeltaEncoder(deltas : List[DeltaValue]) {
  
  /**
   * Adds new DeltaValue to Encoder.
   * @param deltaValue value to be added to encoder.
   * @return new DeltaEncoder with this DeltaValue.
   */
  def addValue(deltaValue : DeltaValue) : DeltaEncoder =
    new DeltaEncoder(deltaValue :: deltas)
  
  /**
   * Encodes delta between current and next list of values.
   * @param values list of next values.
   * @return code and new DeltaEncoder. 
   */
  def delta(values : List[String]) : (Vector[Byte], DeltaEncoder) = {
    val (codes, newDeltas) = deltas.zip(values).map(p => DeltaValue.doDelta(p._2).run(p._1)).unzip
    
    (codes.foldLeft(Vector[Byte]())(_ ++ _), new DeltaEncoder(newDeltas.map(_())))
  }
  
  /**
   * Decodes delta from binary code and current list of values.
   * @param code binary code.
   * @return list of decoded deltas new DeltaEncoder.
   */
  def reverseDelta(code : Vector[Byte]) : (List[String], DeltaEncoder) = {
    def splitCode(deltasTail : List[DeltaValue], c : Vector[Byte]) : List[(DeltaValue, String)] =
      deltasTail match{
        case Nil => Nil
        case d :: ds => {
          val ((value, codeTail), delta) = DeltaValue.doReverseDelta(c).run(d)
          (delta(), value) :: splitCode(ds, codeTail)
        } 
      }
    
    val (newDeltas, values) = splitCode(deltas, code).unzip
    (values, new DeltaEncoder(newDeltas))
  }
}