package Core

/**
 * @brief abstract class DendriteSegment represents segment of connections.
 */
abstract class DendriteSegment {
  
  /**
   * Calculates overlap of the segment over given data. 
   * @param data collection of inputs to the segment.
   * @param get function that gets value from data at specified index.
   * @return overlap value over input data multiplied by boost.
   */
  def overlap[T](data : Vector[T], get : T => Int) : Int
}