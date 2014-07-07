package Core

/**
 * @brief class DistalSegment represents segment of connections, by which column
 * receives prediction from cells.
 * @param connections list of connections.
 */
class DistalSegment(private val m_connections : List[Int]) extends DendriteSegment {
  
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
}