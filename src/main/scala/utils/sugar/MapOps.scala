package utils.sugar

object MapOps {

  def addSeqToMapToSet[A, B](map: Map[A, Set[B]], seq: Seq[(A, B)]): Map[A, Set[B]] = {
    println("add", map, seq)
    val seqMapped: Map[A, Set[B]] = seq.groupBy(_._1)
      .map { case (a, seq) => (a, seq.map(_._2).toSet) }
    val newKeys = seqMapped.keySet &~ map.keySet
    //todo maybe maybe optimize?
    val res = map.map {
      case (a, set) if seqMapped.contains(a) => (a, set | seqMapped(a))
      case (a, set) => (a, set)
    } ++ newKeys.map(k => (k, seqMapped(k)))
    println(res)
    res
  }

}
