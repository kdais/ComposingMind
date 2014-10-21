package Core

/**
 * @brief class Column represents a column of cells. Its main purpose is to group cells which
 * will "receive" same feed-forward input.
 * @param m_firstCell - index of the first column's cell in a region.
 * @param m_proximalSegment - column's proximal segment.
 */
class Column(val firstCell : Int, val proximalSegment : ProximalSegment) {
  /**
   * Percentage of column's synapses.
   * @return percentage of "active" connections.
   */
  def receptiveFieldSize : Float =
    proximalSegment.numOfSynapses.toFloat / proximalSegment.numOfConnections.toFloat
  
  /**
   * Creates a list of active cells in context of region's prediction.
   * @param cells cells of the region.
   * @param nCells number of cells in a column. 
   * @return list of active cells.
   */
  def activeCells(cells : Vector[Cell], nCells : Int) : List[Int] = {
    val myCells = List.range(firstCell, firstCell + nCells, 1)
    val predictedCells = myCells.filter(cells(_).isPredicted(cells, 0))
    
    //If none of the cells is predicted, activate entire column. 
    if (!predictedCells.isEmpty) predictedCells else myCells
  }
  
  /**
   * Calculates overlap of the proximal segment over given data. 
   * @param data collection of inputs to the segment.
   * @return overlap value over input data.
   */
  private def overlapValue(data : Vector[Boolean]) : Int =
    proximalSegment.overlap(data, getData)
  
  /**
   * Updates permanences of connections by adding some value to connections, which
   * contributed to segment activation, and subtracting from those, which did not.
   * @param delta value by which permanences will be updated.
   * @param data collection of inputs to the segment.
   * @return new segment with updated synapses. 
   */
  private def updateConnections(delta : Float, data : Vector[Boolean]) : Column =
    new Column(firstCell, proximalSegment.updatePermanences(delta, data, getData))
    
    private def getData(b : Boolean) = if (b) 1 else 0 
}

object Column {
  /**
   * Feeds the new portion of data to the column.
   * @param data collection of inputs to the segment.
   * @param delta value by which permanences will be updated.
   * @return new State which transitions into overlap value
   * and Column with updated permanences. 
   */
  def overlap(data : Vector[Boolean], delta: Float): State[Column, Int] =
    State((c: Column) => (c.overlapValue(data), State.lazyState(c.updateConnections(delta, data))))
}