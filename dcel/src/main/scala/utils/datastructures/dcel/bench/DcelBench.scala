package utils.datastructures.dcel.bench

import utils.datastructures.dcel.DCEL.{Face, HalfEdge, Vertex}
import utils.datastructures.dcel.PlanarDCELCutPipeline.{CutPoly, PlanarDCELCutPipeline, StepsSeq}
import utils.datastructures.dcel.{DCELDataProvider, PlanarDCEL, PlanarDcelCutPipelineInterpreter}
import utils.datastructures.spatial.AARectangle
import utils.math.planar.V2

/** Micro-benchmark for the original `PlanarDCEL`
  * (`dcel/src/main/scala/utils/datastructures/dcel/`), meant to be compared
  * against the Rust port's equivalent `rust_utils` `benches/dcel_bench.rs`.
  * Run with:
  *   sbt "dcel/runMain utils.datastructures.dcel.bench.DcelBench"
  *
  * Same methodology as `EarcutBench`: `WARMUP_CHUNKS` untimed passes through
  * the exact same loop shape as the `CHUNKS` timed passes that follow.
  *
  * Every accumulator is order-independent - element counts, or areas of
  * integer-coordinate faces - because `DCEL` keeps its elements in
  * identity-hashed sets. Inputs, iteration counts and accumulator
  * definitions are the same as on the Rust side.
  */
object DcelBench {
  val WARMUP_CHUNKS: Long = 15L
  val CHUNKS: Long = 5L

  type D = PlanarDCEL[V2, Int, Int]

  object Provider extends DCELDataProvider[V2, Int, Int] {
    override def newVertexData(v: V2): V2 = v
    override def newFaceData(edge: HalfEdge[V2, Int, Int]): Int = 0
    override def splitEdgeData(edge: HalfEdge[V2, Int, Int], data: V2): (Int, Int) = (0, 0)
    override def newEdgeData(v1: Vertex[V2, Int, Int], v2: Vertex[V2, Int, Int]): (Int, Int) = (0, 0)
  }

  def report(name: String, itersPerChunk: Long, step: Long => Double): Unit = {
    var acc = 0.0
    var c = 0L
    while (c < WARMUP_CHUNKS) {
      val base = c * itersPerChunk
      var j = 0L
      while (j < itersPerChunk) {
        acc += step(base + j)
        j += 1
      }
      c += 1
    }

    print(f"$name%-20s")
    acc = 0.0
    var totalNs = 0.0
    var chunk = 0L
    while (chunk < CHUNKS) {
      val base = (WARMUP_CHUNKS + chunk) * itersPerChunk
      val start = System.nanoTime()
      var j = 0L
      while (j < itersPerChunk) {
        acc += step(base + j)
        j += 1
      }
      val elapsed = (System.nanoTime() - start).toDouble
      totalNs += elapsed
      print(f" ${elapsed / itersPerChunk}%11.1f")
      chunk += 1
    }
    val avg = totalNs / (CHUNKS * itersPerChunk)
    println(f"  | avg $avg%11.1f ns/op   (acc=$acc%.3f)")
  }

  def newDcel(): D = new PlanarDCEL[V2, Int, Int](0, x => x)

  def rect(x0: Double, y0: Double, x1: Double, y1: Double): Seq[V2] =
    AARectangle(V2(x0, y0), V2(x1, y1)).toPolygon.vertices

  /** vertices + half-edges + inner faces */
  def counts(dcel: D): Double = (dcel.vertices.size + dcel.halfEdges.size + dcel.innerFaces.size).toDouble

  /** Sum of the areas of the inner faces' outer contours. */
  def areaSum(dcel: D): Double = dcel.innerFaces.iterator.map(f => dcel.outerContour(f).area).sum

  /** `n x n` adjacent squares of side `size`, cut one by one in row-major order. */
  def cutGrid(n: Int, size: Double): D = {
    val dcel = newDcel()
    for (row <- 0 until n; col <- 0 until n) {
      val x = col * size
      val y = row * size
      dcel.cutPoly(rect(x, y, x + size, y + size), Provider)
    }
    dcel
  }

  // ---- cutGrid: cutPoly of an 8x8 grid of adjacent 10x10 squares ----
  val GRID_N: Int = 8
  val GRID_CELL: Double = 10.0

  def benchCutGrid(): Unit =
    report("cut_grid_8x8", 120, _ => {
      val dcel = cutGrid(GRID_N, GRID_CELL)
      counts(dcel) + areaSum(dcel)
    })

  // ---- cutDisjoint: 64 disjoint 6x6 squares on a 10-pitch 8x8 lattice ----
  def benchCutDisjoint(): Unit =
    report("cut_disjoint_64", 150, _ => {
      val dcel = newDcel()
      for (row <- 0 until 8; col <- 0 until 8) {
        val x = col * 10.0
        val y = row * 10.0
        dcel.cutPoly(rect(x, y, x + 6.0, y + 6.0), Provider)
      }
      counts(dcel) + areaSum(dcel)
    })

  // ---- cutNested: 16 concentric squares, cut from the outside in ----
  val NESTED_K: Int = 16

  def benchCutNested(): Unit =
    report("cut_nested_16", 1500, _ => {
      val dcel = newDcel()
      for (k <- 0 until NESTED_K) {
        val inset = k * 2.0
        dcel.cutPoly(rect(inset, inset, 100.0 - inset, 100.0 - inset), Provider)
      }
      counts(dcel) + areaSum(dcel)
    })

  // ---- cutZigzag: one chain - a 32-point zig-zag, then two horizontal
  // passes back and forth through all of its segments ----
  val ZIGZAG_POINTS: Int = 32

  def makeZigzag(): Seq[V2] = {
    val zig = for (i <- 0 until ZIGZAG_POINTS) yield V2(i * 10.0, if (i % 2 == 0) 0.0 else 100.0)
    val right = ZIGZAG_POINTS * 10.0
    zig ++ Seq(V2(right, 50.0), V2(-10.0, 50.0), V2(-10.0, 25.0), V2(right, 25.0))
  }

  def benchCutZigzag(): Unit = {
    val chain = makeZigzag()
    report("cut_zigzag_chain", 400, _ => {
      val dcel = newDcel()
      val vertices = dcel.cutChain(chain, Provider)
      counts(dcel) + vertices.size.toDouble
    })
  }

  // ---- faceAt: 100 point-location queries on a prebuilt 8x8 grid ----
  val FACE_AT_QUERIES: Long = 100L

  def benchFaceAt(): Unit = {
    val dcel = cutGrid(GRID_N, GRID_CELL)
    val outer = dcel.outerFace
    report("face_at_grid_x100", 300, i => {
      var inside = 0.0
      var q = 0L
      while (q < FACE_AT_QUERIES) {
        // a 10x10 lattice of points with pitch 9.7 starting at -4.877 (so
        // never on a grid line, and partly outside the grid), shifted a
        // little differently every iteration
        val shift = (i % 7).toDouble * 0.01
        val pos = V2((q % 10).toDouble * 9.7 - 4.877 + shift, (q / 10).toDouble * 9.7 - 4.877 + shift)
        if (dcel.faceAt(pos) != outer) inside += 1.0
        q += 1
      }
      inside
    })
  }

  // ---- connectFan: fan of connectVerticesUnsafe over a convex 32-gon ----
  val FAN_N: Int = 32

  def benchConnectFan(): Unit = {
    val ngon: Seq[V2] = for (i <- 0 until FAN_N) yield {
      val t = i.toDouble / FAN_N * 2 * Math.PI
      V2(100.0 * Math.cos(t), 100.0 * Math.sin(t))
    }
    report("connect_fan_32gon", 2000, _ => {
      val dcel = newDcel()
      val vertices = dcel.cutPoly(ngon, Provider).toIndexedSeq
      var k = 2
      while (k < FAN_N - 1) {
        dcel.connectVerticesUnsafe(vertices(0), vertices(k), Provider)
        k += 1
      }
      counts(dcel)
    })
  }

  // ---- mergeGrid: collapse a 6x6 grid into its first cell, row-major ----
  val MERGE_N: Int = 6

  def benchMergeGrid(): Unit =
    report("merge_grid_6x6", 250, _ => {
      val dcel = cutGrid(MERGE_N, GRID_CELL)
      val cells: IndexedSeq[Face[V2, Int, Int]] = for (k <- 0 until MERGE_N * MERGE_N) yield {
        val col = k % MERGE_N
        val row = k / MERGE_N
        dcel.faceAt(V2((col + 0.5) * GRID_CELL, (row + 0.5) * GRID_CELL))
      }
      var merged = 0.0
      for (cell <- cells.tail) {
        if (dcel.mergeAdjancedFaces(cells.head, cell, Provider)) merged += 1.0
      }
      counts(dcel) + merged + dcel.outerContour(cells.head).area
    })

  // ---- cutPipeline: a StepsSeq of labelled CutPoly steps over a 6x6 grid ----
  val PIPELINE_N: Int = 6

  /** One `CutPoly` per cell (row-major): face label = cell index, side labels
    * 10..14 (twins 20..24) and corner labels 0..4 shared by all cells. */
  def makeGridPipeline(): PlanarDCELCutPipeline[V2, Int, Int, Int, Int, Int] =
    StepsSeq[V2, Int, Int, Int, Int, Int](
      (for (k <- 0 until PIPELINE_N * PIPELINE_N) yield {
        val x = (k % PIPELINE_N) * GRID_CELL
        val y = (k / PIPELINE_N) * GRID_CELL
        CutPoly[V2, Int, Int, Int, Int, Int](
          AARectangle(V2(x, y), V2(x + GRID_CELL, y + GRID_CELL)).toPolygon,
          Some(k), Seq(10, 11, 12, 13), Seq(20, 21, 22, 23), Seq(0, 1, 2, 3))
      }): _*)

  def benchCutPipeline(): Unit = {
    val pipeline = makeGridPipeline()
    report("cut_pipeline_6x6", 100, _ => {
      val dcel = newDcel()
      val ctx = PlanarDcelCutPipelineInterpreter.cutPipeline(dcel, Provider, pipeline)
      // produced counts + label-set sizes
      val produced = ctx.vertexProduced.size + ctx.halfEdgesProduced.size + ctx.faceProduced.size
      val labelled = ctx.labelToFace.valuesIterator.map(_.size).sum +
        ctx.labelToHalfEdge.valuesIterator.map(_.size).sum +
        ctx.labelToVertex.valuesIterator.map(_.size).sum
      (produced + labelled).toDouble
    })
  }

  def main(args: Array[String]): Unit = {
    println(s"scalaUtils dcel benchmark - $WARMUP_CHUNKS warmup chunks, then $CHUNKS measured chunks per op, ns per op\n")
    benchCutGrid()
    benchCutDisjoint()
    benchCutNested()
    benchCutZigzag()
    benchFaceAt()
    benchConnectFan()
    benchMergeGrid()

    benchCutPipeline()
  }
}
