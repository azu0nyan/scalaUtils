package ai.behaviorTree

object IfThenElse {
  def apply[D](predicate: () => Boolean, thenNode: BehaviorNode[D], elseNode: BehaviorNode[D], label: String): IfThenElse[D] =
    new IfThenElse(predicate, thenNode, elseNode, Some(label))

  def apply[D](predicate: () => Boolean, thenNode: BehaviorNode[D], elseNode: BehaviorNode[D]): IfThenElse[D] =
    new IfThenElse(predicate, thenNode, elseNode, None)
}

class IfThenElse[D](
                     predicate: () => Boolean,
                     thenNode: BehaviorNode[D],
                     elseNode: BehaviorNode[D],
                     label: Option[String]
                   ) extends BehaviorNode[D] {

  var runing: Option[BehaviorNode[D]] = None

  override def tick(data: D): Status = {
    runing match {
      case Some(node) => run(data, node)
      case None if (predicate.apply()) => run(data, thenNode)
      case None => run(data, elseNode)
    }
  }

  private def run(data: D, node: BehaviorNode[D]): Status = {
    node.tick(data) match {
      case Status.Failure =>
        halt()
        Status.Failure
      case Status.Success =>
        halt()
        Status.Success
      case Status.Running(nodes) =>
        runing = Some(node)
        Status.Running(this +: nodes)
    }
  }

  override def halt(): Unit = {
    for (r <- runing) r.halt()
    runing = None
  }
  override def nodeInfo: String =
    s"IF ${label.getOrElse("")} branch ${if (runing.contains(thenNode)) " then " else if (runing.contains(elseNode)) "else" else "NO BRANCH"}"
}
