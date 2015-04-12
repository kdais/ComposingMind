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
   * Makes prediction what data will be predicted for next n steps.
   * @param steps number of steps to predict.
   * @param activationPercentage how many columns will be activated (0.0 - 1.0 value).
   * @return list of predictions.
   */
  def predictedRawData(steps : Int = 1, activationPercentage : Float) : List[List[String]] = {
    def go(step : Int, region : LowerRegion) : List[List[String]] =
      if (step == steps) Nil
      else {
        val prediction = intListToVector(region.predictedData())
        val (raw, dummy) = region.deltaEncoder.reverseDelta(prediction)
        
        raw :: go(step + 1, region.feedRawData(raw, activationPercentage, false))
      }

    go(0, this)
  }

  private def intListToVector(list : List[Int]) : Vector[Byte] = list match {
    case Nil => Vector.fill(colMapper.numOfCells)(0.toByte)
    case l :: ls => intListToVector(ls).updated(l, 1.toByte)
  }
}