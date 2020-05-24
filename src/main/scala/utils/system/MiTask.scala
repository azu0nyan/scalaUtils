package utils.system

import utils.system.MiTask.MiTaskWithPrice

object MiTask{

  implicit def fromFUU(f:()=> Unit):MiTask = new MiTask {
    override def doTask(): Unit = f()
  }

  implicit def toFUU(task: MiTask):()=> Unit = task.doTask

  implicit def fromSeq(tasks:Seq[MiTask]):MiTask = new MiTask {
    override def price: Int = tasks.map(_.price).sum
    override def doTask(): Unit = tasks.foreach(_.doTask())
  }

  implicit def fromOption(task1: Option[MiTask]):MiTask = task1 match {
    case Some(t) => t
    case None => () => {}
  }

  class MiTaskWithPrice(f:() => Unit, pricee:Int = 0) extends MiTask{
    override def price: Int = pricee
    override def doTask(): Unit = f()
  }

  class Println(s:Any) extends MiTask {
    override def doTask(): Unit = {
      println(s)
    }

    override def price: Int = 1
  }
}

trait MiTask {
  def price:Int = 0

  def doTask ():Unit

  def after(other:MiTask):MiTask = new MiTaskWithPrice(()=>{
    other.doTask()
    this.doTask()
  }, this.price + other.price)

  def before(other:MiTask):MiTask = new MiTaskWithPrice(()=>{
    this.doTask()
    other.doTask()
  }, this.price + other.price)


}


