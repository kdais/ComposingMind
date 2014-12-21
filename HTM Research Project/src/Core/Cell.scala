package Core

/**
 * @brief class Cell represents a single cell in a column that has its unique behavior
 * in terms of column's input and prediction of distal dendrites.
 * @param steps size of history and "prediction".
 * @param stateHistory history of previous states - active or inactive.
 * @param distalSegments vector of distal segments for making prediction.
 */
class Cell(val steps : Int, val stateHistory : List[Boolean], val distalSegments : List[List[DistalSegment]]) {
  
  require(steps > 0 && stateHistory.length == steps && distalSegments.length == steps)
    
  /**
   * Makes an empty cell with history size set to steps.
   * @param steps size of history and "prediction".
   * @return empty cell.
   */
  def this(steps : Int) = this(steps, List.fill(steps)(false), List.fill(steps)(Nil))
  
  /**
   * Makes cell active by pushing active state to its history of states.
   * @return new cell with active state. 
   */
  def makeActive : Cell = new Cell(steps, true :: stateHistory.init, distalSegments)
  
  /**
   * Makes cell inactive by pushing active state to its history of states.
   * @return new cell with inactive state. 
   */
  def makeInactive : Cell = new Cell(steps, false :: stateHistory.init, distalSegments)
  
  /**
   * Checks whether cell was active on a specified step.
   * @param step (0 - current state).
   * @return true if cell was active, or false otherwise. 
   */
  def wasActive(step : Int = 0) : Boolean = stateHistory(step)
  
  /**
   * Checks whether cell was active on any step.
   * @return true if cell was active, or false otherwise. 
   */
  def wasEverActive : Boolean = stateHistory.contains(true)
  
  /**
   * Adds distal segment to cell.
   * @param segment the segment which will be added.
   * @param step prediction step on which segment must be added.
   * @return cell with new segment.
   */
  def addSegment(segment : DistalSegment, step : Int) : Cell = new Cell(steps, stateHistory,
      distalSegments.updated(step, segment :: distalSegments(step)))
  
  /**
   * Checks whether cell is predicted on specified step.
   * @param colMapper vector of cells of the region.
   * @param step on which step we check if cell is predicted.
   * @return true if cell is predicted, or false otherwise.  
   */
  def isPredicted(colMapper : ColumnCellMapper, step : Int) : Boolean =
    distalSegments(step).exists(isSegmentPredicting(_, colMapper, step))
  
  /**
   * Checks whether cell is predicted on any step.
   * @param colMapper columns of the region.
   * @return true if cell is predicted, or false otherwise.
   */
  def isEverPredicted(colMapper : ColumnCellMapper) : Boolean = {
    val stepsRange = 0 until steps
    stepsRange.exists(isPredicted(colMapper, _))
  }
  
  /**
   * Updates segments according to the state of other cells. Removes those which expired.
   * @param colMapper columns of the region.
   * @return new cell with updated distal segments.
   */
  def withUpdatedSegments(colMapper: ColumnCellMapper) : Cell = {
    
    def updateByPrediction(seg : DistalSegment, step : Int) : DistalSegment =
      if (isSegmentPredicting(seg, colMapper, step))
        seg.updateExpirationTime(Constants.DistalExpirationTime / 2)
      else
        seg.updateExpirationTime()
    
    val updatedSegments = for {
      step <- List.range(0, steps)
    } yield distalSegments(step).map(updateByPrediction(_, step)).filter(!_.expired)
    
    new Cell(steps, stateHistory, updatedSegments)
  }
  
  /**
   * Counts number of segments for specific step of prediction.
   * @param step prediction step.
   * @return number of segments on the prediction step.
   */
  def numOfSegments(step : Int) : Int = distalSegments(step).length
    
  private def isSegmentPredicting(seg : DistalSegment,
                                  colMapper : ColumnCellMapper,
                                  step : Int) : Boolean = {
      val indexes = Vector.range(0, colMapper.numOfCells)
      seg.overlap(indexes, (i : Int) => if (colMapper.cell(i).wasActive(step)) 1 else 0).toFloat /
          seg.numOfConnections.toFloat > Constants.DistalPredictionThreshold
    }
}