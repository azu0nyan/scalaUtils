package ai.behaviorTree

enum Status:
  case Success
  case Failure
  case Running(nodes: Seq[BehaviorNode[_]] = Seq())

trait BehaviorNode[-TICKDATA] {
  def tick(data: TICKDATA): Status
  def halt(): Unit = {}
  
  def nodeInfo: String = this.getClass.getSimpleName
}


