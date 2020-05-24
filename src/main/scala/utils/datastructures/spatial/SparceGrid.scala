package utils.datastructures.spatial

import java.util.logging.Level

import utils.Logging
import utils.datastructures.{IntV3, spatial}
import utils.math.Scalar
import utils.math.space.{AABox, Shape, V3}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class SparceGrid[A <: Shape, V](
                                 minPoint:V3,
                                 maxPoint:V3,
                                 cellSize:Scalar = 10f,
                                 cellStorageConstructor: (V3, V3) => SpatialStorage[A, V], //= (_, _) => new spatial.SpatialStorage.NaiveSpatialStorageImplementation[A, V](),
                                 maxVolume:Int  = 1000000
                               ) extends SpatialStorage [A, V]{

  val cellsCount:IntV3 = {
    val tmp = (maxPoint - minPoint) / cellSize
    IntV3(
      tmp.x.toInt + 1,
      tmp.y.toInt + 1,
      tmp.z.toInt + 1
    )
  }
  type CellIndex = Long
  //precalc
  private val dy:CellIndex = cellsCount.x
  private val dz:CellIndex = cellsCount.x * cellsCount.y

  @inline def toCell(v:V3):IntV3 = IntV3((v.x / cellSize).toInt, (v.y / cellSize).toInt,(v.z / cellSize).toInt)

  //@inline def toGridIndex(x:Int, y:Int, z:Int):Int =  x + y * dy + z * dz
  @inline def toGridIndex(x:Int, y:Int, z:Int):CellIndex =  x + y * cellsCount.x + z *  cellsCount.x * cellsCount.y

  def fromCellIndex(i:CellIndex):IntV3 = {
    val x = i % cellsCount.x
    val tmp = (i - x) / cellsCount.x  // y + z *cellsCount.y
    val y = tmp % cellsCount.y
    val z = (tmp - y)  / cellsCount.y
    return IntV3(x.toInt, y.toInt ,z.toInt)
  }

  def rasterizeBox(b:AABox):Array[CellIndex] = {
    val min = toCell(b.min)
    val max = toCell(b.max)
    if((max - min).volume < 0 || (max - min).volume > maxVolume){
      return cells.keys.toArray //TODO redo, only for "values(a)" where area big
     // Logging.logger.log(Level.SEVERE, s"box is to big $min, $max")
    // return Array()
    }
    val a:Array[CellIndex] = new Array[CellIndex]((max - min).volume)
    var id = 0

    var i:Int = min.x
    while(i <= max.x){
      var j:Int = min.y
      while(j <= max.y){
        var k:Int = min.z
        while(k <= max.z){
          a(id) = toGridIndex(i, j, k)
          id += 1
          k += 1
        }
        j += 1
      }
      i += 1
    }
   return a
  }

  def rasterize(a:A):Array[CellIndex] = rasterizeBox(a.aabb)


  val cells:mutable.HashMap[CellIndex, SpatialStorage[A, V]] = new mutable.HashMap()

  //store cells for saved values
  val occupiedCellCache:mutable.HashMap[V, Array[SpatialStorage[A,V]]] = new mutable.HashMap()

  def getStorageById(key:CellIndex):SpatialStorage[A,V] = cells.get(key) match {
    case Some(v) => v
    case None =>
      val min = fromCellIndex(key).toV3 * cellSize
      val max = min + cellSize
      val newStorage = cellStorageConstructor(min, max)
      cells.put(key, newStorage)
      newStorage
  }

  override def add(area: A, value: V): SpatialStorage[A, V] = {
    val rasterized:Array[CellIndex] = rasterize(area)
    var cellsCashe:Array[SpatialStorage[A, V]] = new Array(rasterized.length)
    var i:Int = 0
    while(i < rasterized.length){
      cellsCashe(i) = getStorageById(rasterized(i))
      cellsCashe(i).add(area, value)
      i += 1
    }
    occupiedCellCache.put(value, cellsCashe)
    this
  }

  override def remove(value: V): SpatialStorage[A, V] = {
    occupiedCellCache.remove(value)  match {
      case Some(cellsCached) =>
        var i:Int = 0
        while(i < cellsCached.length){
          cellsCached(i).remove(value)
          i += 1
        }
      case None =>
    }
    this
  }

  override def areas: collection.Seq[A] = cells.values.flatMap(cell => cell.areas).toSeq

  override def values: collection.Seq[V] = occupiedCellCache.keys.toSeq

  override def values(area: A): collection.Seq[V] = {
    val arr = rasterize(area) //todo ANOTHER RASTERIZE
    val res:mutable.HashSet[V] = new mutable.HashSet[V]()
    var i:Int = 0
    while(i < arr.length){
      cells.get(arr(i))  match {
        case Some(storage) => res ++= storage.values(area)
        case None =>
      }
      i += 1
    }
    return res.toSeq
  }
}
