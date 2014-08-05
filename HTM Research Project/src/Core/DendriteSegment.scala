package Core

/**
 * @brief abstract class DendriteSegment represents segment of connections.
 */
abstract class DendriteSegment {
  
  /**
   * Calculates overlap of the segment over given data. 
   * @param data collection of inputs to the segment.
   * @return overlap value over input data multiplied by boost.
   */
  def overlap[T](data : Vector[T], get : T => Int) : Int
  
  /**
   * Returns number of connections of the segment.
   * @return number of connections.
   */
  def numOfConnections : Int
  
  /**
   * Indexes of segment's connections.
   * @return list of indexes. 
   */
  def connectionIndexes : List[Int]
}