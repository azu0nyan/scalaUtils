package utils.sugar

object SeqOps {
  //todo no rec
  def removeConsecutiveDuplicatesCircular[T](s:List[T]):List[T] = {
    var res = removeConsecutiveDuplicates(s)
    while(res.head == res.last){
      res = res.tail
    }
    res 
  }
  
  def removeConsecutiveDuplicates[T](s:List[T]):List[T] = s match {
    case Nil => Nil
    case x :: Nil => List(x)
    case a :: b :: c if a != b => a :: removeConsecutiveDuplicates(b :: c)
    case a :: b :: c => removeConsecutiveDuplicates(b :: c)
    
  }  
}
