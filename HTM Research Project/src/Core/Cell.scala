package Core

/**
 * @brief class Cell represents a single cell in a column that has its unique behavior
 * in terms of column's input and prediction of distal dendrites.
 * @param isActive state - active or inactive.
 * @param distalSegments list of distal segments for making prediction.
 */
class Cell(val isActive : Boolean, val distalSegments : List[DistalSegment]) {
    
  /**
   * Makes an empty cell in inactive state.
   * @return empty cell.
   */
  def this() = this(false, Nil)
  
  /**
   * Makes cell active.
   * @return new cell with active state. 
   */
  def makeActive : Cell = new Cell(true, distalSegments)
  
  /**
   * Makes cell inactive.
   * @return new cell with inactive state. 
   */
  def makeInactive : Cell = new Cell(false, distalSegments)

  /**
   * Adds distal segment to cell.
   * @param segment the segment which will be added.
   * @return cell with new segment.
   */
  def addSegment(segment : DistalSegment) : Cell = new Cell(isActive, segment :: distalSegments)
  
  /**
   * Checks whether cell is predicted.
   * @param colMapper vector of cells of the region.
   * @return true if cell is predicted, or false otherwise.  
   */
  def isPredicted(colMapper : ColumnCellMapper) : Boolean =
    distalSegments.exists(isSegmentPredicting(_, colMapper))
  
  /**
   * Updates segments according to the state of other cells. Removes those which expired.
   * @param colMapper columns of the region.
   * @return new cell with updated distal segments.
   */
  def withUpdatedSegments(colMapper: ColumnCellMapper) : Cell = {
    
    def updateByPrediction(seg : DistalSegment) : DistalSegment =
      if (isSegmentPredicting(seg, colMapper))
        seg.updateExpirationTime(Constants.DistalExpirationTime / 2)
      else
        seg.updateExpirationTime()
    
    val updatedSegments = distalSegments.map(updateByPrediction(_)).filter(!_.expired)
    
    new Cell(isActive, updatedSegments)
  }
  
    
  /**
   * Counts number of segments for specific step of prediction.
   * @param step prediction step.
   * @return number of segments on the prediction step.
   */
  def numOfSegments() : Int = distalSegments.length

    
  private def isSegmentPredicting(seg : DistalSegment, colMapper : ColumnCellMapper) : Boolean = {
      val indexes = Vector.range(0, colMapper.numOfCells)
      seg.overlap(indexes, (i : Int) => if (colMapper.cell(i).isActive) 1 else 0).toFloat /
          seg.numOfConnections.toFloat > Constants.DistalPredictionThreshold
    }
}