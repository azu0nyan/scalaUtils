package utils.math.planar.algo.straightSkeleton

/**
 * An event...that occurs at a specific height
 *
 * Needs to deal with equals and hash for hashsets
 *
 * @author twak
 */

trait HeightEvent {
  def getHeight: Double
  /**
   * Return true if the event takes some action, false otherwise
   */
  def process(skel: Skeleton): Boolean
}

object HeightEvent {
  
  implicit val heightEventOrdering: Ordering[HeightEvent] = Ordering.by(_.getHeight)
  
}
