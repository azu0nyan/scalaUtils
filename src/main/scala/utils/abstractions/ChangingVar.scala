package utils.abstractions

import utils.system.Event
import utils.system.Event.Event
/**Reactive, revisit before use*/
trait ChangingVar[A] {

  def currentOrElse(implicit default:DefaultVariable[A]): A = _current.getOrElse(default.get)

  def current:Option[A] = _current

  def changeToNoneNoCallback():Unit = _current = None

  private var _current: Option[A] = None

  def change(newVal: A): Unit = {
    _current = Some(newVal)
    onChangedEvent.onEvent(newVal)
  }

  private val onChangedEvent: Event[A] = Event[A]

  def addOnChange(f: A => Unit): Unit = onChangedEvent.subscribe(f)

  def filter(f: A => Boolean): ChangingVar[A] = new ChangingVar[A] {
    ChangingVar.this.addOnChange(value => if (f(value)) change(value))
  }

  def optionFilter[B](f: A => Option[B]): ChangingVar[B] = new ChangingVar[B] {
    ChangingVar.this.addOnChange(value => f(value).foreach(newVal =>change(newVal)))
  }

  def map[B](f: A => B): ChangingVar[B] = new ChangingVar[B] {
    ChangingVar.this.addOnChange(value => change(f(value)))
  }
}
