package Core

/**
 * @brief class DendriteSegment represents segment of connections, by which column or cell
 * receives feed-forward input or prediction of other cells from same region.
 * @param connections list of connections and their permanences.
 * @param threshold value of permanence after which synapse is created.
 * @param boost segment overlap multiplier.
 */
class DendriteSegment(private val m_connections : List[(Int, Float)], // (connection, permanence)
					  private val m_threshold : Float,
					  private val m_boost : Int) {
  
  /**
   * Auxiliary constructor with boost set to 1.
   * @param connections list of connections and their permanences.
   * @param threshold value of permanence after which synapse is "created".
   */
  def this(connections : List[(Int, Float)], threshold : Float) = 
    this(connections, threshold, 1)
    
  /**
   * Updates boost of the segment.
   * @param newBoost new boost value.
   * @return new segment with updated boost.
   */
  def withBoost(newBoost : Int) : DendriteSegment =
    new DendriteSegment(m_connections, m_threshold, newBoost)
  
  /**
   * Updates threshold of the segment.
   * @param newThreshold new threshold value.
   * @return new segment with updated threshold.
   */
  def withThreshold(newThreshold: Int) : DendriteSegment =
    new DendriteSegment(m_connections, newThreshold, m_boost)
  
  /**
   * Updates permanences of connections by adding some value to connections, which
   * contributed to segment activation, and subtracting from those, which did not.
   * @param delta value by which permanences will be updated.
   * @param data collection of inputs to the segment.
   * @param get function that gets value from data at specified index.
   * @return new segment with updated synapses. 
   */
  def updatePermanences[T](delta : Float,
		  				   data : Vector[T],
		  				   get : T => Int) : DendriteSegment = {
    
    def toSign(value : Int) : Int = if (value == 0) -1 else 1
    
    def setBounds(permanence : Float) : Float = if (permanence < 0.0F)
      0.0F
    else if (permanence > 1.0F)
      1.0F
    else
      permanence
    
    val updatedConnections = m_connections.map(c => (c._1, setBounds(toSign(get(data(c._1))))))
    new DendriteSegment(updatedConnections, m_threshold, m_boost)
  }
		   
  /**
   * Calculates overlap of the segment over given data. 
   * @param data collection of inputs to the segment
   * @param get function that gets value from data at specified index
   * @return overlap value over input data multiplied by boost
   */
  def overlap[T](data : Vector[T], get : T => Int) : Int = {
    // Choose connections with permanence higher than threshold.
    val synapses = m_connections.filter(_._2 >= m_threshold)
    // Map synapses to their activity values.
    val connectedValues = synapses.map(s => get(data(s._1)))
    connectedValues.sum * m_boost
  }
}

/**
 * @brief object DendriteSegment.
 */
object DendriteSegment {
  
  /**
   * Generates random permanences which are distributed near the threshold.
   * @param length number of connections for which permanences are generated.
   * @param threshold threshold for establishing synapses.
   * @return list with random permanences.
   */
  def genPermanences(length : Int, threshold : Float) : List[Float] = {
    // XXX Add seed to generator.
    val generator = new util.Random()
    
    def genPermanencesRecursive(l : Int) : List[Float] = {
      if (l == 0)
        Nil
      else
        (threshold + ((generator.nextFloat() - 0.5F) / 5.0F)) :: genPermanencesRecursive(l - 1) 
    }
    
    genPermanencesRecursive(length)
  }
}