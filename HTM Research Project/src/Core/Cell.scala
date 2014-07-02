package Core

/**
 * @brief class Cell represents a single cell in a column that has its unique behavior
 * in terms of column's input and prediction of distal dendrites.   
 */
class Cell(val m_state : Int) {
  
  /**
   * Checks whether cell is in active state.
   * @return true if cell is in active state.
   */
  def isActive() : Boolean = (m_state & Cell.Active) != 0
  
  /**
   * Checks whether cell is in predictive state.
   * @return true if cell is in predictive state.
   */
  def isPredictive() : Boolean = (m_state & Cell.Predictive) != 0
  
  /**
   * Turns cell into active state.
   * @return activated cell.
   */
  def makeActive() : Cell = new Cell(m_state | Cell.Active)
  
  /**
   * Turns cell into predictive state.
   * @return predicted cell.
   */
  def makePredictive() : Cell = new Cell(m_state | Cell.Predictive)
}

/**
 * @brief object Cell.
 */
object Cell {
  private val Active = 0x1;
  private val Predictive = 0x2;
}