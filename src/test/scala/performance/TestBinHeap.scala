package performance

import utils.datastructures.containers.BinHeap

import scala.collection.ArrayOps
import scala.util.Random

object TestBinHeap extends App {
  class MyIHeap extends BinHeap[Int]

  for(i<- 0 to 1) {
    for (count <- (0 to (6 + 1)).map(math.pow(10, _).toInt)) {
      val h: BinHeap[Int] = new BinHeap[Int]()
      val elementsAdd: Array[Int] = Random.shuffle(0 until count toBuffer).toArray

      val t1 = new PerformanceTest(() => {
        elementsAdd.foreach(h += _)
        while (h.nonEmpty) h.poll()
      }, s"Heap add $count", true, 1, 1)

      val ih: IntBinHeap = new IntBinHeap()( lt = (x:Int, y:Int) => x < y)
      val t2 = new PerformanceTest(() => {
        elementsAdd.foreach(ih += _)
        while (ih.nonEmpty) ih.poll()
      }, s"IHea add $count", true, 1, 1)

      val mh: MyIHeap = new MyIHeap()
      val t3 = new PerformanceTest(() => {
        elementsAdd.foreach(mh += _)
        while (mh.nonEmpty) mh.poll()
      }, s"MHea add $count", true, 1, 1)



      t2.run()
      t1.run()
     // t3.run()
    }
  }

}
