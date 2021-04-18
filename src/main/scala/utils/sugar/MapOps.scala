package utils.sugar

object MapOps {

  def addSeqToMapToSet[A, B](map:Map[A, Set[B]], seq:Seq[(A, B)]):Map[A, Set[B]] = {
    val seqMapped = seq.toMap
    val newKeys = seqMapped.keySet &~ map.keySet
    //todo maybe maybe optimize?
    map.map{
      case (a,set)  if seqMapped.contains(a) => (a, set ++  Set(seqMapped(a)))
      case (a, set) => (a, set)
    } ++ newKeys.map(k => (k, Set(seqMapped(k))))
  }

}
