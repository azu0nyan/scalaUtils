package ai.behaviorTree

type Pred = PredicateNode

object PredicateNode {
  def apply(code: => Boolean): PredicateNode = new PredicateNode(() => code, None)
  def apply(code: => Boolean, label: String): PredicateNode = new PredicateNode(() => code, Some(label))
}

class PredicateNode(predicate: () => Boolean, label: Option[String]) extends BehaviorNode[Any] {
  var lastPredicateResult: Boolean = true
  override def tick(x: Any): Status =
    lastPredicateResult = predicate()
    if (lastPredicateResult)
      Status.Success
    else
      Status.Failure

  override def nodeInfo: String = label match {
    case Some(l) => s"Predicate: $l : $lastPredicateResult"
    case None =>s"Predicate : $lastPredicateResult"
  }

}



