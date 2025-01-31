package utils.system

import utils.datastructures.IdProvider
import utils.system.Event.Event
import utils.system.SClock.{DiscreteTime, Timestamp}


trait UpdateThread {
  @volatile private var paused = false

  def pause(): Unit = {
    if (!paused) {
      paused = true
      onPaused(():Unit)
    }
  }

  def unPause(): Unit = {
    if (paused) {
      onUnPaused(():Unit)
      paused = false
    }
  }


  @volatile var suspended = false

  def suspend():Unit ={
    suspended = true
  }

  @volatile var sleepBetweenUpdatesMs: Long = 1

  @volatile var sleepWhenPausedMs: Long = 1

  val threadName: String

  def startUpdateThreads(): Thread = {
    val thread:Thread = new Thread(() => {
      updateCycle()
    }, threadName)
    thread.start()
    thread
  }


  def updateCycle(): Unit = {
    onStarted(():Unit)
    var lastTick: Timestamp = SClock.now()
    while (!suspended) {
      if (!paused) {
        lastTick = lastTick.now()
        if (lastTick.dt != 0) {
          update(lastTick)
        }
        if (sleepBetweenUpdatesMs != 0) {
          Thread.sleep(sleepBetweenUpdatesMs)
        }
      } else {
        if (sleepWhenPausedMs != 0) {
          Thread.sleep(sleepWhenPausedMs)
        }
        lastTick = SClock.now()
      }
    }
    onSuspended(():Unit)
  }

  def update(timestamp: SClock.Timestamp): Unit

  val onStarted:Event[Unit] =  Event[Unit]
  val onUnPaused:Event[Unit] =  Event[Unit]
  val onPaused:Event[Unit] =  Event[Unit]
  val onSuspended:Event[Unit] =  Event[Unit]

}
