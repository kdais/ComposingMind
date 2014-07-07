package Core

/**
 * @brief class Column represents a column of cells. Its main purpose is to group cells which will
 * "receive" same feed-forward input.
 * @param cells - 
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
  def overlap(data : Vector[Int]) : Int = {
    m_proximalSegment.overlap(data, (x : Int) => x)
  }
  
  /**
   * Updates vector of region's cells (just those that belong to column) using some function.
   * @param cells vector of region's cells.
   * @param update function that creates modified cell.
   * @return vector with updated cells, that belong to column.
   */
  private def updateCells(cells : Vector[Cell], update : Cell => Cell) : Vector[Cell] = {
    
    def updateCellsRecursive(c : Vector[Cell], index : Int) : Vector[Cell] = {
      if (index == m_firstCell + m_nCells)
        cells
      else
        updateCellsRecursive(cells.updated(index, update(cells(index))), index + 1)
    }
        
    updateCellsRecursive(cells, m_firstCell)
  }
}