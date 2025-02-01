package ai.behaviorTree

import Status.*

object ProducerNode {
  def apply[D](produce: D => BehaviorNode[D]): ProducerNode[D] = new ProducerNode[D](produce, None)
  def apply[D](produce: D => BehaviorNode[D], label: String): ProducerNode[D] = new ProducerNode[D](produce, Some(label))
}

class ProducerNode[D](
                       produce: D => BehaviorNode[D],
                       label: Option[String]
                     ) extends BehaviorNode[D] {
  var runningNode: Option[BehaviorNode[D]] = None

  override def tick(tickData: D): Status = {
    if (runningNode.isEmpty) {
      runningNode = Some(produce(tickData))
    }
    runningNode.get.tick(tickData) match {
      case Success =>
        runningNode = None
        Success
      case Failure =>
        runningNode = None
        Failure
      case Running(nodes) => Running(this +: nodes)
    }
  }

  override def halt(): Unit = {
    runningNode.foreach(_.halt())
    runningNode = None
  }

  override def nodeInfo: String = (label match {
    case Some(l) => s"Producer : $l : "
    case None => s"Producer : "
  }) + (runningNode match {
    case Some(_) => "Running"
    case None => "Halted"
  })
}
