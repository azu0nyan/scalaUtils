package ai.behaviorTree

import Status.*

import scala.annotation.tailrec

/*
Type of ControlNode	|  Child returns FAILURE	| Child returns RUNNING
Sequence          	|  Restart	              |   Tick again
ReactiveSequence  	|  Restart	              |   Restart
ProgressiveSequence |  Tick again	            |   Tick again
*/
trait SequenceNode[-D] extends BehaviorNode[D] {
  def tasks: Seq[BehaviorNode[D]]

  def haltAllChildren(): Unit = tasks.foreach(_.halt())
}

object Sequence {
  def apply[D](tasks: BehaviorNode[D]*): Sequence[D] = new Sequence(tasks: _*)
}

/** Saves progress between ticks */
class Sequence[D](override val tasks: BehaviorNode[D]*) extends SequenceNode[D] {
  var tasksLeft: Seq[BehaviorNode[D]] = tasks

  override def halt(): Unit = {
    tasksLeft = tasks
    haltAllChildren()
  }

  @tailrec final def tickRec(tickData: D): Status = {
    if (tasksLeft.isEmpty)
      halt()
      Success
    else tasksLeft.head.tick(tickData) match {
      case Failure =>
        halt()
        Failure
      case Running(nodes) =>
        Running(this +: nodes)
      case Success =>
        tasksLeft = tasks.tail
        tickRec(tickData)
    }
  }

  final def tick(tickData: D): Status = tickRec(tickData)

  override def nodeInfo: String = s"Sequence ${tasks.size - tasksLeft.size} / ${tasks.size}"
}

object ReactiveSequence {
  def apply[D](tasks: BehaviorNode[D]*): ReactiveSequence[D] = new ReactiveSequence(tasks: _*)
}

/** Starts over every time */
class ReactiveSequence[D](override val tasks: BehaviorNode[D]*) extends SequenceNode[D] {

  override def halt(): Unit = {
    haltAllChildren()
  }

  @tailrec private[this] def tickRec(tickData: D, tasksLeft: Seq[BehaviorNode[D]]): Status = {
    if (tasksLeft.isEmpty)
      halt()
      Success
    else tasksLeft.head.tick(tickData) match {
      case Failure =>
        halt()
        Failure
      case Running(nodes) =>
        //halt subsequent nodes wich can be running since previous ticks
        tasksLeft.tail.foreach(_.halt())
        Running(this +: nodes)
      case Success =>
        tickRec(tickData, tasksLeft.tail)
    }
  }

  override def tick(tickData: D): Status = tickRec(tickData, tasks)

  override def nodeInfo: String = s"ReactiveSequence ${tasks.size}"
}

object ProgressiveSequence {
  def apply[D](tasks: BehaviorNode[D]*): ProgressiveSequence[D] = new ProgressiveSequence(tasks: _*)
}

/** Do not repeats tasks until all taks done. Also known as SequenceStar */
class ProgressiveSequence[D](override val tasks: BehaviorNode[D]*) extends SequenceNode[D] {

  private[this] var tasksLeft: Seq[BehaviorNode[D]] = tasks

  override def halt(): Unit = {
    tasksLeft = tasks
    haltAllChildren()
  }

  @tailrec final def tickRec(tickData: D): Status = {
    if (tasksLeft.isEmpty)
      halt()
      Success
    else tasksLeft.head.tick(tickData) match {
      case Failure => Failure
      case Running(nodes) => Running(this +: nodes)
      case Success =>
        tasksLeft = tasksLeft.tail
        tickRec(tickData)
    }
  }

  override final def tick(tickData: D): Status = tickRec(tickData)

  override def nodeInfo: String = s"Progressive sequence ${tasks.size - tasksLeft.size} / ${tasks.size}"
}

object FallbackSequence {
  def apply[D](tasks: BehaviorNode[D]*): FallbackSequence[D] = new FallbackSequence(tasks: _*)
}

class FallbackSequence[-D](override val tasks: BehaviorNode[D]*) extends SequenceNode[D] {

  private[this] var notFailedTasks: Seq[BehaviorNode[D]] = tasks

  override def halt(): Unit = {
    notFailedTasks = tasks
    haltAllChildren()
  }

  @tailrec final def tickRec(tickData: D): Status = {
    if (notFailedTasks.isEmpty)
      halt()
      Failure
    else notFailedTasks.head.tick(tickData) match {
      case Success =>
        halt()
        Success
      case Running(nodes) => Running(this +: nodes)
      case Failure =>
        notFailedTasks = notFailedTasks.tail
        tickRec(tickData)
    }
  }

  override final def tick(tickData: D): Status = tickRec(tickData)

  override def nodeInfo: String = s"Fallvack Sequence ${tasks.size - notFailedTasks.size} / ${tasks.size}"
}

object ReactiveFallbackSequence {
  def apply[D](tasks: BehaviorNode[D]*): ReactiveFallbackSequence[D] = new ReactiveFallbackSequence(tasks: _*)
}

class ReactiveFallbackSequence[D](override val tasks: BehaviorNode[D]*) extends SequenceNode[D] {

  override def halt(): Unit = {
    haltAllChildren()
  }

  @tailrec private[this] def tickRec(tickData: D, notFailedTasks: Seq[BehaviorNode[D]]): Status = {
    if (notFailedTasks.isEmpty)
      halt()
      Failure
    else notFailedTasks.head.tick(tickData) match {
      case Success =>
        halt()
        Success
      case Running(nodes) =>
        //halt subsequent nodes wich can be running since previous ticks
        notFailedTasks.tail.foreach(_.halt())
        Running(this +: nodes)
      case Failure =>
        notFailedTasks.head.halt()
        tickRec(tickData, notFailedTasks.tail)
    }
  }

  override def tick(tickData: D): Status = tickRec(tickData, tasks)

  override def nodeInfo: String = s"Reactive fallback  ${tasks.size} "
}



