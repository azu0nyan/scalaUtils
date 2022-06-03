package utils.sugar

import scala.annotation.tailrec

object SeqOps {
  //todo no rec
  def removeConsecutiveDuplicatesCircular[T](s:List[T]):List[T] = if(s.nonEmpty){
    var res = removeConsecutiveDuplicates(s)
    while(res.head == res.last){
      res = res.tail
    }
    res 
  } else s
  
  def removeConsecutiveDuplicates[T](s:List[T]):List[T] = s match {
    case Nil => Nil
    case x :: Nil => List(x)
    case a :: b :: c if a != b => a :: removeConsecutiveDuplicates(b :: c)
    case a :: b :: c => removeConsecutiveDuplicates(b :: c)
    
  }


   def insertBeforeCondition[T](s:Seq[T], cond: T => Boolean, toInsert: T): Seq[T] = s match {
    case Nil => List(toInsert)
    case h :: t if cond(h) => toInsert :: h :: t
    case h :: t => h +: insertBeforeCondition(t, cond, toInsert)
  }
}
