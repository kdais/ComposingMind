package Core

/**
 * @brief class DistalSegment represents segment of connections, by which column
 * receives prediction from cells.
 * @param m_connections list of connections.
 * @param m_expirationTime time for the segment to expire its duty.
 */
class DistalSegment(val connections : List[Int],
					val expirationTime : Int) extends DendriteSegment {
  
  require(expirationTime >= 0)
  
  /**
   * Constructor with default expiration time.
   * @param connections list of connection indexes.
   * @return new Region with default expiration time.  
   */
  def this(connections : List[Int]) = this(connections, Constants.DistalExpirationTime)
  
  /**
   * Calculates overlap of the segment over given data. 
   * @param data collection of inputs to the segment.
   * @param get function that gets value from data at specified index.
   * @return overlap value over input data multiplied by boost.
   */
  def overlap[T](data : Vector[T], get : T => Int) : Int = {
    val connectedValues = connections.map(s => get(data(s)))
    connectedValues.sum
  }
  
  /**
   * Returns number of connections of the segment.
   * @return number of connections.
   */
  def numOfConnections : Int = connections.length
  
  /**
   * Indexes of segment's connections.
   * @return list of indexes. 
   */
  def connectionIndexes : List[Int] = connections
  
  /**
   * Updates expiration time of the segment.
   * @param time change of the time that is left for a segment.
   * @return new distal segment with updated expiration time.
   */
  def updateExpirationTime(time : Int = -1) : DistalSegment = {
    assert(expirationTime + time >= 0)
    
    new DistalSegment(connections,
        List(expirationTime + time, Constants.MaxDistalExpirationTime).min)
  }
  
  /**
   * Checks whether segment has expired.
   * @return true if segment expired its time, or false otherwise.
   */
  def expired : Boolean = expirationTime == 0 
}

/**
 * @brief object DistalSegment
 */
object DistalSegment {
  

}