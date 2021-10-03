package utils.math.planar.algo.polygonClipping

import java.util

import utils.math.SMALL_NUMBER
import utils.math.planar.{Polygon, PolygonRegion, V2}

import scala.collection.mutable

/**order of vertices ignored*/
object PolygonClipping {
  implicit val defaultEpsilon: Epsilon = new Epsilon(SMALL_NUMBER)

  def opThenCombine(p1:Polygon, p2:Polygon)(op:(Poly, Poly) => Poly):Polygon = {
    var chains:Seq[Seq[V2]] = op(Poly(p1.regions.map(_.vertices), false), Poly(p2.regions.map(_.vertices), false)).regions.toSeq
    //todo mb better
    def splitChain(chain:Seq[V2]):Seq[Seq[V2]] = {
      val stack:java.util. Stack[V2] = new util.Stack[V2]
      val set:mutable.Set[V2] = new mutable.HashSet[V2]()
      var res:Seq[Seq[V2]] = Nil
      chain.foreach(v => {
        if(set.contains(v)){ // cut chain
          res = {
            var newChain:Seq[V2] = Seq(v)
            while (stack.peek() != v){
              set -= stack.peek()
              newChain = stack.pop() +: newChain
            }
            newChain
          } +: res
        } else {
          stack.push(v)
          set.add(v)
        }
      })
      res = {
        var newChain:Seq[V2] = Seq()
        while (!stack.empty()){
          newChain = stack.pop() +: newChain
        }
        newChain
      } +: res

      return res
    }

    //.regions.map(PolygonRegion(_)).toSeq
    var res = chains.flatMap(splitChain).map(s => PolygonRegion(s))
    res = Polygon.fixOrdersForHoles(res, -1)
    Polygon(res)
  }

  def union:(Polygon, Polygon) => Polygon = opThenCombine(_,_)(PolyBool.union)

  def intersection:(Polygon, Polygon) => Polygon = opThenCombine(_,_)(PolyBool.intersect)

  def difference:(Polygon, Polygon) => Polygon = opThenCombine(_,_)(PolyBool.difference)

  def reverseDifference:(Polygon, Polygon) => Polygon = opThenCombine(_,_)(PolyBool.differenceRev)

  def xor:(Polygon, Polygon) => Polygon = opThenCombine(_,_)(PolyBool.xor)


}
