package Core

/**
 * @brief class Cell represents a single cell in a column that has its unique behavior
 * in terms of column's input and prediction of distal dendrites.
 * @param m_steps size of history and "prediction".
 * @param m_stateHistory history of previous states - active or inactive.
 * @param m_distalSegments vector of distal segments for making prediction.
 */
class Cell(private val m_steps : Int,
		   private val m_stateHistory : Vector[Boolean],
		   private val m_distalSegments : Vector[List[DistalSegment]]) {
  
  require(m_steps > 0 && m_stateHistory.length == m_steps && m_distalSegments.length == m_steps)
  
  /**
   * Makes an empty cell with history size set to steps.
   * @param steps size of history and "prediction".
   * @return empty cell.
   */
  def this(steps : Int) = this(steps, Vector.fill(steps)(false), Vector.fill(steps)(Nil))
  
  /**
   * Makes cell active by pushing active state to its history of states.
   * @return new cell with active state. 
   */
  def makeActive : Cell = new Cell(m_steps, true +: m_stateHistory.init, m_distalSegments)
  
  /**
   * Makes cell inactive by pushing active state to its history of states.
   * @return new cell with inactive state. 
   */
  def makeInactive : Cell = new Cell(m_steps, false +: m_stateHistory.init, m_distalSegments)
  
  /**
   * Checks whether cell was active on a specified step.
   * @param step (0 - current state).
   * @return true if cell was active, or false otherwise. 
   */
  def wasActive(step : Int = 0) : Boolean = m_stateHistory(step)
  
  /**
   * Checks whether cell was active on any step.
   * @return true if cell was active, or false otherwise. 
   */
  def wasEverActive : Boolean = m_stateHistory.contains(true)
  
  /**
   * Adds distal segment to cell.
   * @param segment the segment which will be added.
   * @param step prediction step on which segment must be added.
   * @return cell with new segment.
   */
  def addSegment(segment : DistalSegment, step : Int) : Cell = new Cell(m_steps, m_stateHistory,
      m_distalSegments.updated(step, segment :: m_distalSegments(step)))
  
  /**
   * Checks whether cell is predicted on specified step.
   * @param cells vector of cells of the region.
   * @param step on which step we check if cell is predicted.
   * @return true if cell is predicted, or false otherwise.  
   */
  def isPredicted(cells : Vector[Cell], step : Int) : Boolean =
    m_distalSegments(step).exists(isSegmentPredicting(_, cells))
  
  /**
   * Checks whether cell is predicted on any step.
   * @param cells vector of cells of the region.
   * @return true if cell is predicted, or false otherwise.
   */
  def isEverPredicted(cells : Vector[Cell]) : Boolean = {
    val stepsRange = 0 until m_steps
    stepsRange.exists(isPredicted(cells, _))
  }
  
  /**
   * Updates segments according to the state of other cells. Removes those which expired.
   * @param cells vector of all cells in a region.
   * @return new cell with updated distal segments.
   */
  def withUpdatedSegments(cells : Vector[Cell]) : Cell = {
    
    def updateByPrediction(seg : DistalSegment) : DistalSegment =
      if (isSegmentPredicting(seg, cells))
        seg.updateExpirationTime(DistalSegment.DefaultExpirationTime / 2)
      else
        seg.updateExpirationTime()
        
    val updatedSegments = m_distalSegments.map(_.map(updateByPrediction(_)).filter(!_.expired))
    new Cell(m_steps, m_stateHistory, updatedSegments)
  }
    
  private def isSegmentPredicting(seg : DistalSegment, cells : Vector[Cell]) : Boolean = {
      seg.overlap(cells, (c : Cell) => if (c.wasActive()) 1 else 0).toFloat /
          seg.numOfConnections.toFloat > Cell.PredictionThreshold
    }
}

/**
 * @brief object Cell.
 */
object Cell {
  
  val PredictionThreshold = 0.8F
}