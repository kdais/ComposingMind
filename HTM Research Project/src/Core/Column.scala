package Core

/**
 * @brief class Column represents a column of cells. Its main purpose is to group cells which
 * will "receive" same feed-forward input.
 * @param m_firstCell - index of the first column's cell in a region.
 * @param m_proximalSegment - column's proximal segment.
 */
class Column(val cells : Vector[Cell],
             connections : List[(Int, Float)],// (connection, permanence)
             threshold : Float,
             boost : Int) extends ProximalSegment(connections, threshold, boost) {
  
  def this(cls: Vector[Cell], seg: ProximalSegment) =
    this(cls, seg.connections, seg.threshold, seg.boost)
    
  def apply(i : Int) : Cell = cells(i)
  
  def updateCell(index : Int, f : Cell => Cell) : Column =
    new Column(cells.updated(index, f(cells(index))), connections, threshold, boost)
  
  def updateCells(indexes : List[Int], f : Cell => Cell) : Column = indexes match {
    case Nil => this
    case i :: is => updateCell(i, f).updateCells(is, f) 
  }
  
  def updateCellsIf(p : Cell => Boolean, f : Cell => Cell) : Column =
    updateCells(List.range(0, cells.length).filter(i => p(cells(i))), f)
  
  /**
   * Creates a list of active cells in context of region's prediction.
   * @param cells cells of the region.
   * @param nCells number of cells in a column. 
   * @return list of active cells.
   */
  def activeCells(colMapper: ColumnCellMapper) : List[Int] = {
    val myCells = List.range(0, cells.length, 1)
    val predictedCells = myCells.filter(cells(_).isPredicted(colMapper, 0))
    
    //If none of the cells is predicted, activate entire column.
    if (!predictedCells.isEmpty) predictedCells else myCells
  }
  
  def updateSegment[T](delta : Float, data : Vector[T], get : T => Int) : Column = {
    new Column (cells, updatePermanences(delta, data, get))
  }
}

object Column {
  /**
   * Feeds the new portion of data to the column.
   * @param data collection of inputs to the segment.
   * @param delta value by which permanences will be updated.
   * @return new State which transitions into overlap value
   * and Column with updated permanences. 
   */
  
  private def boolGet = (b: Boolean) => if (b) 1 else 0
  
  def doOverlap(data : Vector[Boolean], delta: Float): State[Column, Int] =
    State((c: Column) => (c.overlap(data, boolGet), State.lazyState(c.updateSegment(delta, data, boolGet))))
}