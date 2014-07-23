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
   * @return list of activated columns.
   */
  def activeColumns(data : Vector[Boolean]) : List[Int] = {
    val overlaps = m_columns.map(_.overlap(data))
    val recepriveFields = m_columns.map(_.receptiveFieldSize)
    val averageReceptiveField = recepriveFields.sum / m_columns.length
    val inhibitionRadius = averageReceptiveField * Region.inhibitionRadiusMultiplier toInt
    
    def manhattanDist(a : (Int, Int), b : (Int, Int)) : Int =
      math.abs(a._1 - b._1) + math.abs(a._2 - b._2)
      
    def neighborsIndexes(index : Int) : Vector[Int] = {
      val allIndexes = Vector.range(0, m_columns.length, 1)
      
      allIndexes.filter(c => manhattanDist(toPoint(index), toPoint(c)) < inhibitionRadius &&
          index != c)
    }
    
    def isOverInhibitionThreshold (index : Int) : Boolean = {
      val neighborsOverlaps = neighborsIndexes(index).map(overlaps(_)).sortWith((a, b) => a > b)
      
      overlaps(index) >= neighborsOverlaps(Region.inhibitionThreshold)
    }
    
    List.range(0, m_columns.length, 1).filter(isOverInhibitionThreshold(_))
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
}

/**
 * @brief object Region.
 */
object Region {
  
  private val inhibitionRadiusMultiplier = 0.5F
  
  private val inhibitionThreshold = 4
}