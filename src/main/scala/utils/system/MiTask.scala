package utils.system


object MiTask{

  implicit def fromFUU(f:()=> Unit):MiTask = () => f()

  implicit def toFUU(task: MiTask):()=> Unit = task.doTask

  implicit def fromSeq(tasks:Seq[MiTask]):MiTask = () => tasks.foreach(_.doTask())

  implicit def fromOption(task1: Option[MiTask]):MiTask = task1 match {
    case Some(t) => t
    case None => () => {}
  }

 /* class MiTaskWithPrice(f:() => Unit, pricee:Int = 0) extends MiTask{
    override def price: Int = pricee
    override def doTask(): Unit = f()
  }*/

  class Println(s:Any) extends MiTask {
    override def doTask(): Unit = {
      println(s)
    }

   // override def price: Int = 1
  }
}

trait MiTask extends Runnable{
  override def run(): Unit = doTask()

   //def price:Int = 0

  def doTask ():Unit

  def after(other:MiTask):MiTask = () => {
    other.doTask()
    MiTask.this.doTask()
  }

  def before(other:MiTask):MiTask = () => {
    MiTask.this.doTask()
    other.doTask()
  }

}


