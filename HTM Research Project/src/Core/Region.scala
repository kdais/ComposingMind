package Core

/**
 * @brief class Region implements spatial pooling and sequence memory.
 * @param m_cells vector of region's cells (for sequence memory).
 * @param m_columns vector of region's columns (for spatial pooling).
 */
class Region(private val m_cells : Vector[Cell],
			 private val m_columns : Vector[Column]) {
  
  require(m_cells.length % m_columns.length == 0)
  
  /**
   * Performs spatial pooling - calculates winning columns over given data.
   * @param data - input vector.
   * @param activationPercentage - how many columns will be activated (0.0 - 1.0 value).
   * @return list of activated columns.
   */
  def activeColumns(data : Vector[Boolean], activationPercentage : Float) : List[Int] = {
    
    assert(activationPercentage > 0.0F && activationPercentage < 1.0F)
    
    val overlaps = m_columns.map(_.overlap(data))
    val recepriveFields = m_columns.map(_.receptiveFieldSize)
    // Average percentage of synapses in columns.
    val averageReceptiveField = recepriveFields.sum / m_columns.length
    // Region's diagonal multiplied by avarage percentage of synapses. 
    val inhibitionRadius = (m_regionEdgeSize *  averageReceptiveField *
      Region.inhibitionRadiusMultiplier).toInt
    // Number of columns that are allowed to be active inside inhibition radius.
    val inhibitionThreshold = m_columns.length * activationPercentage toInt
    
    def manhattanDist(a : (Int, Int), b : (Int, Int)) : Int = {
      math.abs(a._1 - b._1) + math.abs(a._2 - b._2)
    }
      
    def neighborsIndexes(index : Int) : Vector[Int] = {
      val allIndexes = Vector.range(0, m_columns.length, 1)
      
      allIndexes.filter(c => manhattanDist(toPoint(index), toPoint(c)) < inhibitionRadius &&
          index != c)
    }
    
    def isOverInhibitionThreshold (index : Int) : Boolean = {
      val neighborsOverlaps = neighborsIndexes(index).map(overlaps(_)).sortWith((a, b) => a > b)
      
      overlaps(index) >= neighborsOverlaps(inhibitionThreshold)
    }
    
    List.range(0, m_columns.length, 1).filter(isOverInhibitionThreshold(_))
  }
  
  /**
   * Adjusts region's columns by modifying their permanences of those, which were
   * activated due to given input.
   * @param data region's input.
   * @param activeCols list of activated columns.
   * @return new Region with columns, adjusted to given input.
   */
  def adjustToInput(data : Vector[Boolean], activeCols : List[Int]) : Region = {
    
    def adjustColumns(toUpdate : List[Int]) : Vector[Column] =
      if (toUpdate.isEmpty)
        m_columns
      else
        adjustColumns(toUpdate.tail).
          updated(toUpdate.head, m_columns(toUpdate.head).
              updateConnections(Constants.ProximalAdjustDelta, data))

    new Region(m_cells, adjustColumns(activeCols))
  }
  
  /**
   * Looks at what cells will be activated due to current input in context of previous state.
   * @param activeCols list of active columns.
   * @return list of indexes of activated cells.
   */
  def activeCells(activeCols : List[Int]) : List[Int] = {
    val cellLists : List[List[Int]] = 
      for (i <- activeCols) yield m_columns(i).activeCells(m_cells, m_cellsPerColumn)

    foldIndexes(cellLists)
  }
  
  /**
   * @brief Number of cells in one column.
   */
  private val m_cellsPerColumn : Int = m_cells.length / m_columns.length
  
  /**
   * @brief Size of the edge of region.
   */
  private val m_regionEdgeSize : Int = math.sqrt(m_columns.length).toInt
  
  /**
   * Converts vector index into 2D position.
   * @param index index in one dimensional vector. 
   * @return 2D position.
   */
  private def toPoint(index : Int) : (Int, Int) =
    (index / m_regionEdgeSize, index % m_regionEdgeSize)
    
  /**
   * Converts 2D position into vector index.
   * @param point 2D position. 
   * @return index in one dimensional vector.
   */
  private def toIndex(point : (Int, Int)) : Int = point._1 * m_regionEdgeSize + point._2
  
  /**
   * Merges all indexes that are contained in a separate lists.
   * @param lists list of lists of indexes.
   * @return list containing all those indexes. 
   */
  private def foldIndexes(lists : List[List[Int]]) : List[Int] =
    lists.foldLeft(List[Int]())(_ ::: _)
}

/**
 * @brief object Region.
 */
object Region {
  
  /**
   * Generates a region with with "clear" cells and columns.
   * @param dataLength length of the input collection.
   * @param regionEdge length of region's edge (in columns).
   * @param cellsPerColumn - number of cells in one column.
   */
  def genRegion(dataLength : Int, regionEdge : Int, cellsPerColumn : Int) : Region = {
    val numOfColumns = regionEdge * regionEdge
    val numOfCells = numOfColumns * cellsPerColumn
    val cellIndexes = Vector.range(0, numOfCells, cellsPerColumn)
    
    val columns = cellIndexes.map(
        new Column(_, ProximalSegment.genDefaultSegment(dataLength / 2, dataLength)))
    val cells = Vector.range(0, numOfCells, 1).map(i => new Cell(Constants.CellDefaultSteps))
    
    new Region(cells, columns)
  }
  
  private val inhibitionRadiusMultiplier = 1.4F
}