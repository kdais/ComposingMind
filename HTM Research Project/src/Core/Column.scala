package Core

/**
 * @brief class Column represents a column of cells. Its main purpose is to group cells which
 * will "receive" same feed-forward input.
 * @param m_firstCell - index of the first column's cell in a region.
 * @param m_proximalSegment - column's proximal segment.
 */
class Column(val firstCell : Int,
			 val proximalSegment : ProximalSegment) {
  
  /**
   * Calculates overlap of the proximal segment over given data. 
   * @param data collection of inputs to the segment.
   * @return overlap value over input data.
   */
  def overlap(data : Vector[Boolean]) : Int =
    proximalSegment.overlap(data, getData)
  
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
   * Updates permanences of connections by adding some value to connections, which
   * contributed to segment activation, and subtracting from those, which did not.
   * @param delta value by which permanences will be updated.
   * @param data collection of inputs to the segment.
   * @return new segment with updated synapses. 
   */
  def updateConnections(delta : Float, data : Vector[Boolean]) : Column =
    new Column(firstCell,
    		   proximalSegment.updatePermanences(delta, data, getData))
  
  /**
   * Percentage of column's synapses.
   * @return percentage of "active" connections.
   */
  def receptiveFieldSize : Float =
    proximalSegment.numOfSynapses.toFloat / proximalSegment.numOfConnections.toFloat
    
    private def getData(b : Boolean) = if (b) 1 else 0 
}