package Core

/**
 * @brief class ProximalSegment represents segment of connections, by which column
 * receives feed-forward input.
 * @param connections list of connections and their permanences.
 * @param threshold value of permanence after which synapse is created.
 * @param boost segment overlap multiplier.
 */
class ProximalSegment(private val m_connections : List[(Int, Float)],// (connection, permanence)
					  private val m_threshold : Float, 
					  private val m_boost : Int) extends DendriteSegment {
  
  /**
   * Auxiliary constructor with boost set to 1.
   * @param connections list of connections and their permanences.
   * @param threshold value of permanence after which synapse is "created".
   */
  def this(connections : List[(Int, Float)], threshold : Float) =  
    this(connections, threshold, 1)
    
  /**
   * Segment's boost.
   * @return boost value of the segment.
   */
  def boost : Int = m_boost
    
  /**
   * Updates boost of the segment.
   * @param newBoost new boost value.
   * @return new segment with updated boost.
   */
  def withBoost(newBoost : Int) : ProximalSegment =
    new ProximalSegment(m_connections, m_threshold, newBoost)
  
  /**
   * Segment's threshold.
   * @return threshold value of the segment.
   */
  def threshold : Float = m_threshold
  
  /**
   * Updates threshold of the segment.
   * @param newThreshold new threshold value.
   * @return new segment with updated threshold.
   */
  def withThreshold(newThreshold: Int) : ProximalSegment =
    new ProximalSegment(m_connections, newThreshold, m_boost)
  
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
		  				   get : T => Int) : ProximalSegment = {
    
    def toSign(value : Int) : Int = if (value == 0) -1 else 1
    
    def setBounds(permanence : Float) : Float = if (permanence < 0.0F)
      0.0F
    else if (permanence > 1.0F)
      1.0F
    else
      permanence
    
    val updatedConnections = m_connections.map(c => 
      (c._1, setBounds(c._2 + delta * toSign(get(data(c._1))))))
    new ProximalSegment(updatedConnections, m_threshold, m_boost)
  }
		   
  /**
   * Calculates overlap of the segment over given data. 
   * @param data collection of inputs to the segment.
   * @param get function that gets value from data at specified index.
   * @return overlap value over input data multiplied by boost.
   */
  def overlap[T](data : Vector[T], get : T => Int) : Int = {
    // Map synapses to their activity values.
    val connectedValues = synapses.map(s => get(data(s._1)))
    connectedValues.sum * m_boost
  }
  
  /**
   * Returns number of connections of the segment.
   * @return number of connections.
   */
  def numOfConnections : Int = m_connections.length
  
  /**
   * Returns number of synapses of the segment.
   * @return number of synapses.
   */
  def numOfSynapses : Int = synapses.length
  
  /**
   * Takes connections with permanence higher than threshold.
   * @return list of synapses.
   */
  private def synapses : List[(Int, Float)] = m_connections.filter(_._2 >= m_threshold)
}

/**
 * @brief object DendriteSegment.
 */
object ProximalSegment {
  
  /**
   * Generates random permanences which are distributed near the threshold.
   * @param length number of connections for which permanences are generated.
   * @param threshold threshold for establishing synapses.
   * @param radius maximum distance of distribution for generated values.
   * @return list with random permanences.
   */
  def genPermanences(length : Int, threshold : Float, radius : Float) : List[Float] = {
    
    assert(radius >= 0.0F && radius <= 0.5F)
    // XXX Add seed to generator.
    val generator = new util.Random()
    
    def genPermanencesRecursive(l : Int) : List[Float] = {
      if (l == 0)
        Nil
      else
        (threshold + ((generator.nextFloat() - 0.5F) * 2.0F * radius)) ::
        genPermanencesRecursive(l - 1)
    }
    
    genPermanencesRecursive(length)
  }
}