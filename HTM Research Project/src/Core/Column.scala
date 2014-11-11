package Core

/**
 * @brief class Column represents a column of cells. Its main purpose is to group cells which
 * will "receive" same feed-forward input.
 * @param cells Vector of cells.
 * @param connections list of connections and their permanences.
 * @param threshold value of permanence after which synapse is created.
 * @param boost segment overlap multiplier.
 */
class Column(val cells : Vector[Cell],
             connections : List[(Int, Float)],// (connection, permanence)
             threshold : Float,
             boost : Int) extends ProximalSegment(connections, threshold, boost) {
  
  /**
   * Constructs Column from cells and ProximalSegment.
   * @param cls Vector of cells.
   * @param seg parent object of ProximalSegment 
   */
  def this(cls: Vector[Cell], seg: ProximalSegment) =
    this(cls, seg.connections, seg.threshold, seg.boost)
    
  /**
   * Access i-th Cell.
   * @param i index of needed Cell.
   * @return i-th cell.
   */
  def apply(i : Int) : Cell = cells(i)
  
  /**
   * Updates i-th Cell by applying f to it.
   * @param i index of a Cell.
   * @param f function for modifying Cell.
   * @return new ColumnCellMapper with updated i-th Cell.  
   */
  def updateCell(index : Int, f : Cell => Cell) : Column =
    new Column(cells.updated(index, f(cells(index))), connections, threshold, boost)
  
  /**
   * Updates Cells by applying f to them.
   * @param indexes positions of Cells to update.
   * @param f function for modifying Cell.
   * @return new ColumnCellMapper with updated Cells.  
   */
  def updateCells(indexes : List[Int], f : Cell => Cell) : Column = indexes match {
    case Nil => this
    case i :: is => updateCell(i, f).updateCells(is, f) 
  }
  
  /**
   * Updates Cells by applying f to those which satisfy p.
   * @param p boolean predicate.
   * @param f function for modifying Cell.
   * @return new ColumnCellMapper with updated Cells.  
   */
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
  
  /**
   * Updates permanences of connections by adding some value to connections, which
   * contributed to segment activation, and subtracting from those, which did not.
   * @param delta value by which permanences will be updated.
   * @param data collection of inputs to the segment.
   * @param get function that gets value from data at specified index.
   * @return new Column with updated synapses. 
   */
  def updateSegment[T](delta : Float, data : Vector[T], get : T => Int) : Column = {
    new Column(cells, updatePermanences(delta, data, get))
  }
}

/**
 * @brief Column object.
 */
object Column {  
  private def byteGet = (b: Byte) => b.toInt
  
  /**
   * Feeds the new portion of data to the column.
   * @param data collection of inputs to the segment.
   * @param delta value by which permanences will be updated.
   * @return new State which transitions into overlap value
   * and Column with updated permanences. 
   */
  def doOverlap(data : Vector[Byte], delta: Float): State[Column, Int] =
    State((c: Column) => (c.overlap(data, byteGet), State.lazyState(c.updateSegment(delta, data, byteGet))))
}