package Core

/**
 * @brief class DistalSegment represents segment of connections, by which column
 * receives prediction from cells.
 * @param m_connections list of connections.
 * @param m_expirationTime time for the segment to expire its duty.
 */
class DistalSegment(private val m_connections : List[Int],
					private val m_expirationTime : Int) extends DendriteSegment {
  
  require(m_expirationTime >= 0)
  
  /**
   * Calculates overlap of the segment over given data. 
   * @param data collection of inputs to the segment.
   * @param get function that gets value from data at specified index.
   * @return overlap value over input data multiplied by boost.
   */
  def overlap[T](data : Vector[T], get : T => Int) : Int = {
    val connectedValues = m_connections.map(s => get(data(s)))
    connectedValues.sum
  }
  
  /**
   * Returns number of connections of the segment.
   * @return number of connections.
   */
  def numOfConnections : Int = m_connections.length
  
  /**
   * Updates expiration time of the segment.
   * @param time change of the time that is left for a segment.
   * @return new distal segment with updated expiration time.
   */
  def updateExpirationTime(time : Int = -1) : DistalSegment = {
    assert(m_expirationTime + time >= 0)
    
    new DistalSegment(m_connections,
        List(m_expirationTime + time, DistalSegment.MaxExpirationTime).min)
  }
  
  /**
   * Checks whether segment has expired.
   * @return true if segment expired its time, or false otherwise.
   */
  def expired : Boolean = m_expirationTime == 0 
}

/**
 * @brief object DistalSegment
 */
object DistalSegment {
  
  val DefaultExpirationTime = 10
  val MaxExpirationTime = 20
}