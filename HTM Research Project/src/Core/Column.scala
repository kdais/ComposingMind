package Core

/**
 * @brief class Column represents a column of cells. Its main purpose is to group cells which
 * will "receive" same feed-forward input.
 * @param m_firstCell - index of the first column's cell in a region.
 * @param m_nCells - number of cells in a column.
 * @param m_proximalSegment - column's proximal segment.
 */
class Column(private val m_firstCell : Int,
			 // XXX maybe remove nCells from here because it is common for all columns in region.
			 private val m_nCells : Int,
			 private val m_proximalSegment : ProximalSegment) {
  
  /**
   * Calculates overlap of the proximal segment over given data. 
   * @param data collection of inputs to the segment.
   * @return overlap value over input data.
   */
  def overlap(data : Vector[Int]) : Int = m_proximalSegment.overlap(data, (x : Int) => x)
  
  /**
   * Creates a list of active cells in context of region's prediction.
   * @param cells - cells of the region.
   * @return list of active cells.
   */
  def activeCells(cells : Vector[Cell]) : List[Int] = {
    val myCells = List.range(m_firstCell, m_firstCell + m_nCells, 1)
    val predictedCells = myCells.filter(cells(_).isEverPredicted(cells))
    
    //If none of the cells is predicted, activate entire column. 
    if (!predictedCells.isEmpty) predictedCells else myCells
  }
  
  /**
   * Updates permanences of connections by adding some value to connections, which
   * contributed to segment activation, and subtracting from those, which did not.
   * @param delta value by which permanences will be updated.
   * @param data collection of inputs to the segment.
   * @return new segment with updated synapses. 
   */
  def updateConnections(delta : Float, data : Vector[Int]) : Column =
    new Column(m_firstCell, m_nCells,
        m_proximalSegment.updatePermanences(delta, data, (x : Int) => x))
  
  /**
   * Percentage of column's synapses.
   * @return percentage of "active" connections.
   */
  def receptiveFieldSize : Float =
    m_proximalSegment.numOfSynapses.toFloat / m_proximalSegment.numOfConnections.toFloat
}