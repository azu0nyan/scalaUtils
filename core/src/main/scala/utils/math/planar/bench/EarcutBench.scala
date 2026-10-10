package utils.math.planar.bench

import utils.math.planar.V2
import utils.math.planar.algo.polygonTriangulation.Triangulation

/** Micro-benchmark for the original `polygonTriangulation.Earcut`/
  * `Triangulation` (`core/src/main/scala/utils/math/planar/algo/polygonTriangulation/`),
  * meant to be compared against the Rust rewrite's equivalent `math_core`
  * `benches/earcut_bench.rs`. Run with:
  *   sbt "core/runMain utils.math.planar.bench.EarcutBench"
  *
  * Same methodology as `V2Bench`/`GeometryBench`: `WARMUP_CHUNKS` untimed
  * passes through the exact same loop shape as the `CHUNKS` timed passes
  * that follow — see `V2Bench`'s doc comment for why that distinction
  * matters on the JVM.
  */
object EarcutBench {
  val WARMUP_CHUNKS: Long = 15L
  val CHUNKS: Long = 5L

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

    print(f"$name%-16s")
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
      print(f" ${elapsed / itersPerChunk}%9.1f")
      chunk += 1
    }
    val avg = totalNs / (CHUNKS * itersPerChunk)
    println(f"  | avg $avg%9.1f ns/op   (acc=$acc%.3f)")
  }

  // ---- earcutSimple: 256-point wavy blob, no holes (same shape as GeometryBench's convex_hull input) ----
  val BLOB_POINTS: Int = 256

  def makeBlob(seedOffset: Double): IndexedSeq[V2] =
    for (i <- 0 until BLOB_POINTS) yield {
      val t = (i.toDouble + seedOffset) / BLOB_POINTS * 2 * Math.PI
      val r = 20.0 + 6.0 * Math.sin(7.0 * t)
      V2(r * Math.cos(t), r * Math.sin(t))
    }

  def benchEarcutSimple(): Unit = {
    val BLOB_VARIANTS = 64
    val blobs = for (i <- 0 until BLOB_VARIANTS) yield makeBlob(i * 0.37)
    report(
      "earcut_simple",
      2000,
      i => Triangulation.triangulate(blobs((i % BLOB_VARIANTS).toInt), Seq()).triangulationIndices.size.toDouble,
    )
  }

  // ---- earcutWithHole: square donut, outer + one square hole ----
  def benchEarcutWithHole(): Unit = {
    val outer = Seq(V2(-20.0, -20.0), V2(20.0, -20.0), V2(20.0, 20.0), V2(-20.0, 20.0))
    val hole = Seq(Seq(V2(-10.0, -10.0), V2(10.0, -10.0), V2(10.0, 10.0), V2(-10.0, 10.0)))
    report(
      "earcut_with_hole",
      200000,
      _ => Triangulation.triangulate(outer, hole).triangulationIndices.size.toDouble,
    )
  }

  def main(args: Array[String]): Unit = {
    println(s"scalaUtils earcut benchmark - $WARMUP_CHUNKS warmup chunks, then $CHUNKS measured chunks per op\n")
    benchEarcutSimple()
    benchEarcutWithHole()
  }
}
