package utils.datastructures.dcel

import utils.datastructures.CircullarOps
import utils.datastructures.dcel.DCEL.{DCELData, Face, HalfEdge, Vertex}
import utils.math.planar.PolygonRegion

import scala.collection.mutable

object DCELOps {

  def selectToTheLeft[D <: DCELData](hes: Seq[HalfEdge[D]]): Iterator[Face[D]] = new Iterator[Face[D]] {

    val visitedFaces = new mutable.HashSet[Face[D]]()
    val forbiddenEdges: Set[HalfEdge[D]] = hes.toSet
    val faceQueue: mutable.Queue[Face[D]] = mutable.Queue()

    hes.foreach(he => {
      if (!visitedFaces.contains(he.leftFace)) {
        faceQueue += he.leftFace
        visitedFaces += he.leftFace
      }
    })

    override def hasNext: Boolean = faceQueue.nonEmpty
    override def next(): Face[D] = {
      val res = faceQueue.dequeue()

      res.edges
        .filter(he => !forbiddenEdges.contains(he))
        .map(x => x.leftFace.asInstanceOf[Face[D]])
        .foreach { face =>
          if (!visitedFaces.contains(face)) {
            faceQueue += face
            visitedFaces += face
          }
        }

      res
    }
  }

  def toClosedChain[D <: DCELData](vs: Seq[Vertex[D]]): Iterator[HalfEdge[D]] = toClosedChainOpt(vs).flatten

  def toClosedChainOpt[D <: DCELData](vs: Seq[Vertex[D]]): Iterator[Option[HalfEdge[D]]] =
    if (vs.size >= 2) CircullarOps.toCyclicPairs(vs).map{case (a,b )=> a.edgeTo(b)}
    else Iterator.empty


  def toChainOpt[D <: DCELData](vs: Seq[Vertex[D]]): Iterator[Option[HalfEdge[D]]] =
    if (vs.size >= 2) vs.sliding(2).map {
      case List(o: Vertex[D], e: Vertex[D]) => o.edgeTo(e)
    } else Iterator.empty

  def toChain[D <: DCELData](vs: Seq[Vertex[D]]): Iterator[HalfEdge[D]] = toChainOpt(vs).flatten


}
