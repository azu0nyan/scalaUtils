package utils.abstractions

import utils.system.Event
import utils.system.Event.Event
object EnabledDisabled {
  implicit def toBoolean(ed:EnabledDisabled):Boolean = ed.enabledVar

}
class EnabledDisabledImpl(override val initialEnabled: Boolean)extends EnabledDisabled

trait EnabledDisabled {

  val initialEnabled :Boolean = true

  private var enabledVar:Boolean = initialEnabled

  var onEnabled:Event[Unit] = Event[Unit]

  var onDisabled:Event[Unit] = Event[Unit]

  def enable():Unit  = {
    if(!enabledVar){
      onEnabled(():Unit)
    }
    enabledVar = true
  }

  def disable():Unit = {
    if(enabledVar){
      onDisabled(():Unit)
    }
    enabledVar = false
  }

  def enabled:Boolean = enabledVar

  def toggle():this.type = {
    if(enabled) disable()
    else enable()

    this
  }
}