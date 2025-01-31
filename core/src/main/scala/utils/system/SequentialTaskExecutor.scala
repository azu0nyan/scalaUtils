package utils.system

import java.util.concurrent.ConcurrentLinkedQueue


/*
class PricedSequentialTaskExecutor[T <:MiTask](var pricePerUpdate: Int = 2000,
                             var priceLimit: Int = 8000,
                             var money: Int = 0,
                             var maxTasks: Int = 50
                            ) {
  private val tasks: ConcurrentLinkedQueue[T] = new ConcurrentLinkedQueue[T]()

  def addTask(t: T): Boolean = tasks.add(t)

  def executeSomeTasks(price: Int = priceLimit) = {
    var taskExecuted = 0
    money += pricePerUpdate

    while (!tasks.isEmpty && (tasks.size() > maxTasks || tasks.peek().price < money)) {
      val task = tasks.poll()
      task.doTask()
      money -= task.price
      taskExecuted += 1
    }
    money = math.max(0, money)

    if (money >= priceLimit) {
      money = priceLimit
      if (taskExecuted == 0 && !tasks.isEmpty) {
        tasks.poll().doTask()
        money == 0
      }
    }
  }
}*/
class SimpleSequentialTaskExecutor[T <:Runnable]() {
  private val tasks: ConcurrentLinkedQueue[T] = new ConcurrentLinkedQueue[T]()

  def addTask(t: T): Boolean = tasks.add(t)

  def executeAllTasks():Unit = {
    while (!tasks.isEmpty) {
      val task = tasks.poll()
      task.run()

    }
  }
}