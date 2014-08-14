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
    // Region's diagonal multiplied by average percentage of synapses.
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
   * Adjusts region's columns by modifying their of those, which were
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
   * Updates cells's history according to which cells were activated (used when learning is
   * turned off).
   * @param activeCells list of activated cells.
   * @return new Region with updated history.
   */
  def withUpdatedHistory(activeCells : List[Int]) : Region = {
    val updatedCells = for {
      i <- Vector.range(0, m_cells.length, 1)
    } yield if (activeCells.contains(i)) m_cells(i).makeActive else m_cells(i).makeActive
    
    new Region(updatedCells, m_columns)
  }
  
  /**
   * Returns cells to which new segment must be added.
   * @param activeCells cells activated due to the input.
   * @return list of cells' indexes.
   */
  def learningCells(activeCells : List[Int]) : List[Int] = {
    
    def numOfAllSegments(cell : Cell, step : Int) : Int=
      if (step == cell.steps) 0 else numOfAllSegments(cell, step + 1) + cell.numOfSegments(step)
    
    //Take one cell from "unpredicted column" with least segments.
    for {
      index <- List.range(0, m_columns.length, 1)
      if (index until index + m_cellsPerColumn).forall(activeCells.contains(_))
    } yield (index until index + m_cellsPerColumn).
             minBy((i : Int) => numOfAllSegments(m_cells(i), 0))
  }
  
  /**
   * Adds distal segments to learning cells.
   * @param learningCells indexes of cells to which distal segments must be added.
   * @return new region with updated cells.
   */
  def withUpdatedPrediction(learningCells : List[Int]) : Region = {

    lazy val activeCellsOnSteps : List[List[Int]] = for {
      step <- List.range(0, m_cells(0).steps, 1)
    } yield activeCellsOnStep(step)

    val segments : List[DistalSegment] = activeCellsOnSteps.
      takeWhile(!_.isEmpty).map(new DistalSegment(_))

    def withSegments(cell : Cell, segs : List[DistalSegment], step : Int) : Cell = segs match {
      case Nil => cell
      case s :: ss => withSegments(cell.addSegment(s, step), ss, step + 1)
    }
    
    def addSegmentsToCells(indexes : List[Int]) : Vector[Cell] = indexes match {
      case Nil => m_cells
      case ix :: ixs => {
        val newCells = addSegmentsToCells(ixs)
        newCells.updated(ix, withSegments(newCells(ix), segments, 0))
      }
    }

    new Region(addSegmentsToCells(learningCells), m_columns)
  }
  
  /**
   * Returns cells which were active on specified step of history.
   * @param step step of history.
   * @return list of cells' indexes.
   */
  def activeCellsOnStep(step : Int) : List[Int] = filterCellsOnStep(_.wasActive(step))

  /**
   * Returns cells which were predictive on specified step of history.
   * @param step step of prediction.
   * @return list of cells' indexes.
   */
  def predictiveCellsOnStep(step : Int) : List[Int] =
    filterCellsOnStep(_.isPredicted(m_cells, step))

  /**
   * Region's cells.
   */
  def cells : Vector[Cell] = m_cells
  
  /**
   * @brief Number of cells in one column.
   */
  private val m_cellsPerColumn : Int = m_cells.length / m_columns.length
  
  /**
   * @brief Size of the edge of region.
   */
  private val m_regionEdgeSize : Int = math.sqrt(m_columns.length).toInt
  
  /**
   * Returns cells that satisfy predicate.
   * @param predicate boolean predicate that takes Cell as parameter.
   * @return list of cells' indexes.
   */
  private def filterCellsOnStep(predicate : Cell => Boolean) : List[Int] =
    List.range(0, m_cells.length, 1).filter((i : Int) => predicate(m_cells(i)))
  
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
        //XXX
    val cells = Vector.range(0, numOfCells, 1).map(i => new Cell(Constants.CellDefaultSteps))
    
    new Region(cells, columns)
  }
  
  private val inhibitionRadiusMultiplier = 1.4F
}