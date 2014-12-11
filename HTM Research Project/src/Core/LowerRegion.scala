package Core

/**
 * @brief class LowerRegion a partial case of region that receives raw data.
 */
class LowerRegion(val deltaEncoder : DeltaEncoder, colMapper : ColumnCellMapper) extends Region(colMapper) {
  
  /**
   * converts raw data to byte vector and feeds it to region.
   * @param data raw data in String representation.
   * @param activationPercentage how many columns will be activated (0.0 - 1.0 value).
   * @param learnOn indicates whether region is learning new connections.
   * @return new Region with remembered history and updated connections if
   * learning is turned on.
   */
  def feedRawData(data : List[String], activationPercentage : Float, learnOn : Boolean) : LowerRegion = {
    val (code, encoder) = deltaEncoder.delta(data)
    
    new LowerRegion(encoder, feedData(code, activationPercentage, learnOn).colMapper)
  }

  /**
   * Makes prediction to all steps.
   * @return list of predictions.
   */
  def predictedRawData() : List[List[String]] = {
    def go(step : Int, encoder : DeltaEncoder) : List[List[String]] =
      if (step == colMapper.numOfSteps) Nil
      else {
        val prediction = intListToVector(predictiveColumnsOnStep(step))
        val (raw, newEncoder) = encoder.reverseDelta(prediction)
        
        raw :: go(step + 1, newEncoder)
      }

    go(1, deltaEncoder)
  }

  private def intListToVector(list : List[Int]) : Vector[Byte] = list match {
    case Nil => Vector.fill(colMapper.numOfCells)(0.toByte)
    case l :: ls => intListToVector(ls).updated(l, 1.toByte)
  }
}