package ai.behaviorTree

object ReactiveIfThenElse {
  def apply[D](predicate: () => Boolean, thenNode: BehaviorNode[D], elseNode: BehaviorNode[D], label: String): ReactiveIfThenElse[D] =
    new ReactiveIfThenElse(predicate, thenNode, elseNode, Some(label))

  def apply[D](predicate: () => Boolean, thenNode: BehaviorNode[D], elseNode: BehaviorNode[D]): ReactiveIfThenElse[D] = 
    new ReactiveIfThenElse(predicate, thenNode, elseNode, None)
}

class ReactiveIfThenElse[D](
                             predicate: () => Boolean,
                             thenNode: BehaviorNode[D],
                             elseNode: BehaviorNode[D],
                             label: Option[String]
                           ) extends BehaviorNode[D] {

  var runing: Option[BehaviorNode[D]] = None

  override def tick(data: D): Status = {
    if (predicate.apply()) {
      if (runing.contains(elseNode)) {
        runing.get.halt()
      }
      runing = None
      thenNode.tick(data) match {
        case Status.Running(nodes) =>
          runing = Some(thenNode)
          Status.Running(this +: nodes)
        case x =>
          thenNode.halt()
          x
      }
    } else {
      if (runing.contains(thenNode)) {
        runing.get.halt()
      }
      runing = None
      elseNode.tick(data) match {
        case Status.Running(nodes) =>
          runing = Some(elseNode)
          Status.Running(this +: nodes)
        case x =>
          elseNode.halt()
          x
      }
    }
  }


  override def halt(): Unit = {
    for (r <- runing) r.halt()
    runing = None
  }
  override def nodeInfo: String =
    s"REACTIVE IF ${label.getOrElse("")} branch ${if (runing.contains(thenNode)) " then " else if (runing.contains(elseNode)) "else" else "NO BRANCH"}"
}

