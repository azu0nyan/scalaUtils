package utils.datastructures.dcel

import utils.datastructures.CircullarOps
import utils.datastructures.dcel.DCEL.{DCELData, Face, HalfEdge, Vertex}
import utils.math.planar.PolygonRegion

import scala.collection.mutable

object DCELOps {

  def selectToTheLeft[VD, HD, FD](hes: Seq[HalfEdge[VD, HD, FD]]): Iterator[Face[VD, HD, FD]] = new Iterator[Face[VD, HD, FD]] {

    val visitedFaces = new mutable.HashSet[Face[VD, HD, FD]]()
    val forbiddenEdges: Set[HalfEdge[VD, HD, FD]] = hes.toSet
    val faceQueue: mutable.Queue[Face[VD, HD, FD]] = mutable.Queue()

    hes.foreach(he => {
      if (!visitedFaces.contains(he.leftFace)) {
        faceQueue += he.leftFace
        visitedFaces += he.leftFace
      }
    })

    override def hasNext: Boolean = faceQueue.nonEmpty
    override def next(): Face[VD, HD, FD] = {
      val res = faceQueue.dequeue()

      res.edges
        .filter(he => !forbiddenEdges.contains(he))
        .map(x => x.leftFace.asInstanceOf[Face[VD, HD, FD]])
        .foreach { face =>
          if (!visitedFaces.contains(face)) {
            faceQueue += face
            visitedFaces += face
          }
        }

      res
    }
  }

  def toClosedChain[VD, HD, FD](vs: Seq[Vertex[VD, HD, FD]]): Iterator[HalfEdge[VD, HD, FD]] = toClosedChainOpt(vs).flatten

  def toClosedChainOpt[VD, HD, FD](vs: Seq[Vertex[VD, HD, FD]]): Iterator[Option[HalfEdge[VD, HD, FD]]] =
    if (vs.size >= 2) CircullarOps.toCyclicPairs(vs).map{case (a,b )=> a.edgeTo(b)}
    else Iterator.empty


  def toChainOpt[VD, HD, FD](vs: Seq[Vertex[VD, HD, FD]]): Iterator[Option[HalfEdge[VD, HD, FD]]] =
    if (vs.size >= 2) vs.sliding(2).map {
      case List(o: Vertex[VD, HD, FD], e: Vertex[VD, HD, FD]) => o.edgeTo(e)
    } else Iterator.empty

  def toChain[VD, HD, FD](vs: Seq[Vertex[VD, HD, FD]]): Iterator[HalfEdge[VD, HD, FD]] = toChainOpt(vs).flatten


}
