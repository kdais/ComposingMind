package Core

/**
 * @brief class Region implements spatial pooling and sequence memory.
 * @param colMapper wrapper over columns vector.
 */
class Region(val colMapper : ColumnCellMapper) {
  
  /**
   * Returns cells which were active on specified step of history.
   * @param step step of history.
   * @return list of cells' indexes.
   */
  def activeCellsOnStep(step : Int) : List[Int] = filterCells(_.wasActive(step))

  /**
   * Returns cells which were predictive on specified step of history.
   * @param step step of prediction.
   * @return list of cells' indexes.
   */
  def predictiveCellsOnStep(step : Int) : List[Int] =
    filterCells(_.isPredicted(colMapper, step))

  /**
   * Feeds a new portion of data to a region.
   * @param data input vector.
   * @param activationPercentage how many columns will be activated (0.0 - 1.0 value).
   * @param learnOn indicates whether region is learning new connections.
   * @return new Region with remembered history and updated connections if
   * learning is turned on.
   */
  def feedData(data : Vector[Boolean], activationPercentage : Float, learnOn : Boolean) : Region = {
    val (overlaps, newColumns) = (for {
      i <- Vector.range(0, colMapper.numOfColumns)
    } yield Column.doOverlap(data, Constants.ProximalAdjustDelta).run(colMapper(i))).unzip
    val activeCols = activeColumns(overlaps, activationPercentage)
    val activeCls = activeCells(activeCols)
    val updatedRegion = withUpdatedHistory(activeCls)
    
    if (learnOn)
      updatedRegion.withUpdatedPrediction(learningCells(activeCls)).adjustToInput(newColumns, activeCols)
    else
      updatedRegion
  }

  /**
   * Performs spatial pooling - calculates winning columns over given data.
   * @param overlaps overlap value of each column.
   * @param activationPercentage - how many columns will be activated (0.0 - 1.0 value).
   * @return list of activated columns.
   */
  private def activeColumns(overlaps : Vector[Int], activationPercentage : Float) : List[Int] = {
    
    assert(activationPercentage > 0.0F && activationPercentage < 1.0F)
    
    val recepriveFields = for (i <- Vector.range(0, colMapper.numOfColumns)) yield colMapper(i).receptiveFieldSize
    // Average percentage of synapses in columns.
    val averageReceptiveField = recepriveFields.sum / colMapper.numOfColumns
    // Region's diagonal multiplied by average percentage of synapses.
    val inhibitionRadius = (m_regionEdgeSize *  averageReceptiveField *
      Region.inhibitionRadiusMultiplier).toInt
    // Number of columns that are allowed to be active inside inhibition radius.
    val inhibitionThreshold = colMapper.numOfColumns * activationPercentage toInt
    
    def manhattanDist(a : (Int, Int), b : (Int, Int)) : Int = {
      math.abs(a._1 - b._1) + math.abs(a._2 - b._2)
    }
      
    def neighborsIndexes(index : Int) : Vector[Int] = {
      val allIndexes = Vector.range(0, colMapper.numOfColumns, 1)
      
      allIndexes.filter(c => manhattanDist(toPoint(index), toPoint(c)) < inhibitionRadius &&
          index != c)
    }
    
    def isOverInhibitionThreshold (index : Int) : Boolean = {
      val neighborsOverlaps = neighborsIndexes(index).map(overlaps(_)).sortWith((a, b) => a > b)
      
      overlaps(index) >= neighborsOverlaps(inhibitionThreshold)
    }
    
    List.range(0, colMapper.numOfColumns).filter(isOverInhibitionThreshold(_))
  }
  
  /**
   * Adjusts region's columns by modifying their of those, which were
   * activated due to given input.
   * @param newColumns new columns with updated connections.
   * @param activeCols list of activated columns.
   * @return new Region with columns, adjusted to given input.
   */
  private def adjustToInput(newColumns : Vector[State.LazyState[Column]], activeCols : List[Int]) : Region = {
    def go(toUpdate : List[Int], mapper : ColumnCellMapper) : ColumnCellMapper = toUpdate match {
      case Nil => colMapper
      case i :: is => go(is, mapper.updateColumn(i, c => newColumns(i)()))
    }
    
    new Region(go(activeCols, colMapper))
  }
  
  /**
   * Looks at what cells will be activated due to current input in context of previous state.
   * @param activeCols list of active columns.
   * @return list of indexes of activated cells.
   */
  private def activeCells(activeCols : List[Int]) : List[Int] = {
    val cellLists : List[List[Int]] = 
      for (i <- activeCols) yield colMapper(i).activeCells(colMapper)

    foldIndexes(cellLists)
  }
  
  /**
   * Updates cells's history according to which cells were activated (used when learning is
   * turned off).
   * @param activeCells list of activated cells.
   * @return new Region with updated history.
   */
  private def withUpdatedHistory(activeCells : List[Int]) : Region =     
    new Region(colMapper.updateCells(activeCells, _.makeActive).
        updateCells(List.range(0, colMapper.numOfCells).diff(activeCells), _.makeInactive))
  
  /**
   * Returns cells to which new segment must be added.
   * @param activeCells cells activated due to the input.
   * @return list of cells' indexes.
   */
  private def learningCells(activeCells : List[Int]) : List[Int] = {
    
    def numOfAllSegments(cell : Cell, step : Int) : Int =
      if (step == cell.steps) 0 else numOfAllSegments(cell, step + 1) + cell.numOfSegments(step)
    
    //Take one cell from "unpredicted column" with least segments.
    for {
      cells <- List.range(0, colMapper.numOfColumns).map(i => i until i + colMapper.cellsPerColumn)
      if cells.forall(activeCells.contains(_))
    } yield cells.minBy((i : Int) => numOfAllSegments(colMapper.cell(i), 0))
  }
  
  /**
   * Adds distal segments to learning cells.
   * @param learningCells indexes of cells to which distal segments must be added.
   * @return new region with updated cells.
   */
  private def withUpdatedPrediction(learningCls : List[Int]) : Region = {

    val activeCellsOnSteps : List[List[Int]] = for {
      step <- List.range(0, colMapper.cell(0).steps)
    } yield activeCellsOnStep(step)

    val segments : List[DistalSegment] = activeCellsOnSteps.
      takeWhile(!_.isEmpty).map(new DistalSegment(_))

    def addSegments(cell : Cell, segs : List[DistalSegment], step : Int) : Cell = segs match {
      case Nil => cell
      case s :: ss => addSegments(cell.addSegment(s, step), ss, step + 1)
    }

    new Region(colMapper.updateCells(learningCls, c => addSegments(c, segments, 0).withUpdatedSegments(colMapper)))
  }
  
  /**
   * @brief Size of the edge of region.
   */
  private val m_regionEdgeSize : Int = math.sqrt(colMapper.numOfColumns).toInt
  
  /**
   * Returns cells that satisfy predicate.
   * @param predicate boolean predicate that takes Cell as parameter.
   * @return list of cells' indexes.
   */
  private def filterCells(predicate : Cell => Boolean) : List[Int] =
    List.range(0, colMapper.numOfCells, 1).filter((i : Int) => predicate(colMapper.cell(i)))
  
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
  private val inhibitionRadiusMultiplier = 1.4F
}