package Core

/**
 * @brief class for handling list of delta values.
 * @param 
 */
class DeltaEncoder(deltas : List[DeltaValue[Any]]) {
  
  /**
   * Adds new DeltaValue to Encoder.
   * @param deltaValue value to be added to encoder.
   * @return new DeltaEncoder with this DeltaValue.
   */
  def addValue(deltaValue : DeltaValue[Any]) : DeltaEncoder =
    new DeltaEncoder(deltaValue :: deltas)
  
  /**
   * Encodes delta between current and next list of values.
   * @param values list of next values.
   * @return code and new DeltaEncoder. 
   */
  def delta(values : List[Any]) : (Vector[Byte], DeltaEncoder) = {
    val (codes, newDeltas) = deltas.zip(values).map(p => DeltaValue.doDelta(p._2).run(p._1)).unzip
    
    (codes.foldLeft(Vector[Byte]())(_ ++ _), new DeltaEncoder(newDeltas.map(_())))
  }
  
  /**
   * Decodes delta from binary code and current list of values.
   * @param code binary code.
   * @return list of decoded deltas new DeltaEncoder.
   */
  def reverseDelta(code : Vector[Byte]) : (List[Any], DeltaEncoder) = {
    def splitCode(deltasTail : List[DeltaValue[Any]], c : Vector[Byte]) : List[(DeltaValue[Any], Any)] =
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