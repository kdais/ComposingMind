package Core

/**
 * @brief Constant values for HTM implementation.
 */
object Constants {
  
  /**
   * @brief percentage of synapses in dendrite segment, needed to activate one.
   */
  val DistalPredictionThreshold = 0.8F
  
  /**
   * @brief number of steps that cell can remember and predict.
   */
  val CellDefaultSteps = 3
  
  /**
   * @brief default expiration time for a distal segment.
   */
  val DistalExpirationTime = 10
  
  /**
   * @brief maximum expiration time for a distal segment.
   */
  val MaxDistalExpirationTime = 20
  
  /**
   * @brief activation threshold for proximal segment's connections.
   */
  val ProximalThreshold = 0.5F
  
  /**
   * @brief radius in which permanences are distributed around ProximalThreshold
   *  after their generation.
   */
  val ProximalThresholdRadius = 0.2F
  
  /**
   * @brief value by which permanences of active columns' segments are updated. 
   */
  val ProximalAdjustDelta = 0.05F
  
  /**
   * @brief default number of cells in region's square's edge.
   */
  val RegionEdge = 30
}