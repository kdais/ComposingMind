package Core

class LowerRegion(val deltaEncoder : DeltaEncoder, colMapper : ColumnCellMapper) extends Region(colMapper) {
  
  def feedRawData(data : List[String], activationPercentage : Float, learnOn : Boolean) : LowerRegion = {
    val (code, encoder) = deltaEncoder.delta(data)
    
    new LowerRegion(encoder, feedData(code, activationPercentage, learnOn).colMapper)
  }
  
  def predictedRawData() : List[List[String]] = {
    def go(step : Int, encoder : DeltaEncoder) : List[List[String]] =
      if (step == colMapper.numOfSteps) Nil
      else {
        val prediction = intListToVector(predictiveCellsOnStep(step))
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