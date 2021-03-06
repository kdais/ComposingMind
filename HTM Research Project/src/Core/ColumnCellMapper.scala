package Core

/**
 * @brief class ColumnCellMapper wrapper for columns that allows working cells,
 * contained in columns as well as with columns.
 * @param columns Vector of columns.
 * @param cellsPerColumn number of cells in each Column. 
 */
class ColumnCellMapper (val columns : Vector[Column], val cellsPerColumn: Int) { 

  /**
   * Access i-th Column.
   * @param i index of Column.
   * @return i-th Column.
   */
  def apply(i : Int) : Column = columns(i)
  
  /**
   * Access i-th Column.
   * @param i index of Column.
   * @return i-th Column.
   */
  def col(i : Int) : Column = this(i)
  
  /**
   * Access i-th Cell.
   * @param i index of Cell.
   * @return i-th Cell.
   */
  def cell(i : Int) : Cell = {
    val (iColumn, iCell) = cellIndex(i)
    this(iColumn)(iCell)
  }
  
  /**
   * Maps list of selected columns to their synapses.
   * @param indexes columns' indexes.
   * @return indexes of input data, to which selected
   * columns established synapse connections.
   */
  def synapsesIndexes(indexes : List[Int]) : List[Int] =
    indexes.map(columns(_).synapseIndexes).foldLeft(List[Int]())(_ ::: _).distinct
  
  /**
   * Updates i-th Column by applying f to it.
   * @param i index of a Column.
   * @param f function for modifying Column.
   * @return new ColumnCellMapper with updated i-th Column.  
   */
  def updateColumn(i : Int, f : Column => Column) : ColumnCellMapper =
    new ColumnCellMapper(columns.updated(i, f(columns(i))), cellsPerColumn)
  
  /**
   * Updates Columns by applying f to them.
   * @param indexes positions of Columns to update.
   * @param f function for modifying Column.
   * @return new ColumnCellMapper with updated Columns.  
   */
  def updateColumns(indexes : List[Int], f : Column => Column) : ColumnCellMapper = indexes match {
    case Nil => this
    case i :: is => updateColumn(i, f).updateColumns(is, f)
  } 
  
  /**
   * Updates Columns by applying f to those which satisfy p.
   * @param p boolean predicate.
   * @param f function for modifying Column.
   * @return new ColumnCellMapper with updated Columns.  
   */
  def updateColumnsIf(p: Column => Boolean, f : Column => Column) =
    new ColumnCellMapper(columns.map(c => if (p(c)) c else f(c)), cellsPerColumn)
  
  /**
   * Updates i-th Cell by applying f to it.
   * @param i index of a Cell.
   * @param f function for modifying Cell.
   * @return new ColumnCellMapper with updated i-th Cell.  
   */
  def updateCell(i : Int, f : Cell => Cell) : ColumnCellMapper = {
    val (iColumn, iCell) = cellIndex(i)
    updateColumn(iColumn, _.updateCell(iCell, f))
  }
  
  /**
   * Updates Cells by applying f to them.
   * @param indexes positions of Cells to update.
   * @param f function for modifying Cell.
   * @return new ColumnCellMapper with updated Cells.  
   */
  def updateCells(indexes : List[Int], f : Cell => Cell) : ColumnCellMapper = indexes match {
    case Nil => this
    case i :: is => updateCell(i, f).updateCells(is, f)
  }
  
  /**
   * Updates Cells by applying f to those which satisfy p.
   * @param p boolean predicate.
   * @param f function for modifying Cell.
   * @return new ColumnCellMapper with updated Cells.  
   */
  def updateCellsIf(p : Cell => Boolean, f : Cell => Cell) : ColumnCellMapper =
    new ColumnCellMapper(columns.map(_.updateCellsIf(p, f)), cellsPerColumn)
  
  /**
   * Total number of columns
   */
  val numOfColumns = columns.length
  
  /**
   * Total number of cells.
   */
  val numOfCells = numOfColumns * cellsPerColumn
  
  private def cellIndex(i : Int) = (i / cellsPerColumn, i % cellsPerColumn)
}