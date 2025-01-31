package utils.math.planar.algo.polygonTriangulation

import utils.datastructures.{IndexedTriangle, IndexedTriangle2}
import utils.math.planar.{Polygon, TrianglePlanar, V2}

import scala.collection.mutable.ArrayBuffer
object Triangulation {

  trait Triangulation {
    val vertices: Seq[V2]

    val triangulationIndices: Seq[(Int, Int, Int)]
//    val triangulationIndicesFlat: Seq[Int] = triangulationIndices.flatMap{case (a,b,c) => Seq(a,b,c)}

    def triangulationIndexedTriangles: Iterable[IndexedTriangle2] = triangulationIndices.map(ti => IndexedTriangle2(ti, vertices.apply))

    def triangles: Iterable[TrianglePlanar] = triangulationIndexedTriangles.map(_.triangle)

    def combineWith(ot: Triangulation): Triangulation = new Triangulation {
      override val vertices: Seq[V2] = Triangulation.this.vertices ++ ot.vertices

      private def sizeOff: Int = Triangulation.this.vertices.size

      override val triangulationIndices: Seq[(Int, Int, Int)] = Triangulation.this.triangulationIndices ++ ot.triangulationIndices
        .map(tri => (tri._1 + sizeOff, tri._2 + sizeOff, tri._3 + sizeOff))
    }
  }

  def triangulate(p:Polygon):Triangulation = {
    val polyAndHoles = Polygon.toRegionsAndHoles(p.regions)
    polyAndHoles.map(ph => triangulate(ph._1.vertices, ph._2.map(_.vertices))).reduceOption((t1, t2)=>t1.combineWith(t2)).getOrElse(new Triangulation {
      override val vertices: Seq[V2] = Seq()
      override val triangulationIndices: Seq[(Int, Int, Int)] = Seq()
    })
  }

  def triangulate(poly:Seq[V2], holes:Seq[Seq[V2]]):Triangulation = {
    /**
      * Triangulates the given polygon
      *
      *  data        is a flat array of vertice coordinates like [x0,y0, x1,y1, x2,y2, ...].
      *  holeIndices is an array of hole indices if any (e.g. [5, 8] for a 12-vertice input would mean one hole with vertices 5–7 and another with 8–11).
      *  dim         is the number of coordinates per vertice in the input array
      * return List containing groups of three vertice indices in the resulting array forms a triangle.
      */

    val dim = 2
    val data:ArrayBuffer[Double] = new ArrayBuffer[Double]()
    val holeIndices:ArrayBuffer[Int ] = new ArrayBuffer[Int]()
    poly.foreach(v => {
      data.append(v.x)
      data.append(v.y)
    })
    holes.foreach(h =>{
      holeIndices.append(data.size / 2)
      h.foreach(v =>{
        data.append(v.x)
        data.append(v.y)
      })
    })

    import scala.jdk.CollectionConverters.*
    val tris:Seq[(Int,Int,Int)] = Earcut.earcut(data.toArray, holeIndices.toArray, dim).asScala.grouped(3).map(t =>(t(0).toInt, t(1).toInt, t(2).toInt)).toSeq
   // println(tris)
    new Triangulation {
      override val vertices: Seq[V2] = data.grouped(2).map(v => V2(v(0), v(1))).toSeq
      override val triangulationIndices: Seq[(Int, Int, Int)] = tris
    }

  }


}

