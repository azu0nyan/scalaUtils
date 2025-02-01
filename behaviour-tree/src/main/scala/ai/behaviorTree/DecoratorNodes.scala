package ai.behaviorTree

import Status.*

trait DecoratorNode[D] extends BehaviorNode[D]

object Invert{
  def apply[D](task:BehaviorNode[D]):Invert[D] = new Invert[D](task)
}

class Invert[D](task: BehaviorNode[D]) extends DecoratorNode[D] {
  override def tick(tickData: D): Status = task.tick(tickData) match {
    case Running(nodes) => Running(this +: nodes)
    case Success => Failure
    case Failure => Success
  }
  override def halt(): Unit = {
    task.halt()
  } 
}

object ForceSuccess{
  def apply[D](task:BehaviorNode[D]):ForceSuccess[D] = new ForceSuccess[D](task)
}

class ForceSuccess[D](task: BehaviorNode[D]) extends DecoratorNode[D] {
  override def tick(tickData: D): Status = task.tick(tickData) match {
    case Running(nodes) => Running(this +: nodes)
    case Success | Failure => Success
  }
  override def halt(): Unit = {
    task.halt()
  }  
}

object ForceFailure{
  def apply[D](task:BehaviorNode[D]):ForceFailure[D] = new ForceFailure[D](task)
}

class ForceFailure[D](task: BehaviorNode[D]) extends DecoratorNode[D] {
  override def tick(tickData: D): Status = task.tick(tickData) match {
    case Running(nodes) => Running(this +: nodes)
    case Success | Failure => Failure
  }
  override def halt(): Unit = {
    task.halt()
  }
}

object Repeat{
  def apply[D](task:BehaviorNode[D], count: Int):Repeat[D] = new Repeat[D](task, count)
}

class Repeat[D](task: BehaviorNode[D], count: Int) extends DecoratorNode[D] {
  var repeat: Int = 0
  override def tick(tickData: D): Status = {
    var res = Success
    while (res == Success && repeat < count) {
      res = task.tick(tickData)
      if (res == Success)
        repeat += 1
    }
    res match {
      case Success | Failure =>
        repeat = 0
        res
      case Running(nodes) => Running(this +: nodes)
    }
  }
  override def halt(): Unit = {
    task.halt()
    repeat = 0
  }

  override def nodeInfo: String = s"Repeating $repeat / $count"
}

object Retry{
  def apply[D](task:BehaviorNode[D], count: Int):Retry[D] = new Retry[D](task, count)
}

class Retry[D](task: BehaviorNode[D], count: Int) extends DecoratorNode[D] {
  var retry: Int = 0
  override def tick(tickData: D): Status = {
    var res = Failure
    while (res == Failure && retry < count) {
      res = task.tick(tickData)
      if (res == Failure)
        retry += 1
    }
    res match {
      case Success | Failure =>
        retry = 0
      case _ =>
    }
    res
  }

  override def halt(): Unit = {
    task.halt()
    retry = 0
  }

  override def nodeInfo: String = s"Retrying $retry / $count"
}

