package Core

/**
 * @brief class DendriteSegment represents segment of connections, by which column or cell
 * receives feed-forward input or active states of other cells from same region.
 */
class DendriteSegment(private val connections : Vector[Int], private val boost : Int) {
  
  /**
   * Calculates overlap of the segment over given data. 
   * @param data collection of inputs to the segment
   * @param get function that gets value from data at specified index
   * @return overlap value over input data multiplied by boost
   */
  def overlap[T](data : Vector[T], get : (Vector[T], Int) => Int) : Int = {
    val connectedValues = connections.map(get(data, _))  // choose values, connected to segment 
    connectedValues.sum * boost
  }
  
  /**
   * Updates boost of the segment.
   * @param newBoost new boost value
   * @return new segment with updated boost
   */
  def withBoost(newBoost : Int) : DendriteSegment = new DendriteSegment(connections, newBoost)
}