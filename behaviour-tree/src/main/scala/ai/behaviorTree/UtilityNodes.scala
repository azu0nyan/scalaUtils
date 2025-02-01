package ai.behaviorTree

case object AlwaysFailNode extends BehaviorNode[Any]{
  override def tick(x:Any): Status = Status.Failure
}

case object AlwaysSuccessNode extends BehaviorNode[Any]{
  override def tick(x:Any): Status = Status.Success
}

case object AlwaysRunningNode extends BehaviorNode[Any]{
  override def tick(x:Any): Status = Status.Running(Seq())
}
