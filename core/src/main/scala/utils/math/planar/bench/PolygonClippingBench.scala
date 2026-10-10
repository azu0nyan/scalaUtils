package utils.math.planar.bench

import utils.math.planar.algo.polygonClipping.PolygonClipping
import utils.math.planar.{Polygon, PolygonRegion, V2}

/** Micro-benchmark for the original `polygonClipping.PolyBool`
  * (`core/src/main/scala/utils/math/planar/algo/polygonClipping/`), meant to
  * be compared against the Rust rewrite's equivalent `math_core`
  * `benches/polygon_clipping_bench.rs`. Run with:
  *   sbt "core/runMain utils.math.planar.bench.PolygonClippingBench"
  *
  * Same methodology as `V2Bench`/`GeometryBench`/`EarcutBench`:
  * `WARMUP_CHUNKS` untimed passes through the exact same loop shape as the
  * `CHUNKS` timed passes that follow.
  */
object PolygonClippingBench {
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
      print(f" ${elapsed / itersPerChunk}%9.1f")
      chunk += 1
    }
    val avg = totalNs / (CHUNKS * itersPerChunk)
    println(f"  | avg $avg%9.1f ns/op   (acc=$acc%.3f)")
  }

  def rect(x0: Double, y0: Double, x1: Double, y1: Double): PolygonRegion =
    PolygonRegion(Seq(V2(x0, y0), V2(x1, y0), V2(x1, y1), V2(x0, y1)))

  // ---- clipSquares: two overlapping axis-aligned squares (cheap baseline, few intersections) ----

  def benchClipSquares(): Unit = {
    val p1 = Polygon(Seq(rect(0, 0, 10, 10)))
    val p2 = Polygon(Seq(rect(5, 0, 15, 10)))
    report("clip_squares", 20000, _ => PolygonClipping.union(p1, p2).area)
  }

  // ---- clipBlobs: two overlapping 48-point wavy blobs, offset from each
  // other — many segment/segment intersections. ----

  val BLOB_POINTS: Int = 48

  def makeBlob(centerX: Double, seedOffset: Double): PolygonRegion = {
    val vs = for (i <- 0 until BLOB_POINTS) yield {
      val t = (i.toDouble + seedOffset) / BLOB_POINTS * 2 * Math.PI
      val r = 20.0 + 6.0 * Math.sin(7.0 * t)
      V2(centerX + r * Math.cos(t), r * Math.sin(t))
    }
    PolygonRegion(vs)
  }

  def benchClipBlobs(): Unit = {
    val VARIANTS = 32
    val blobsA = for (i <- 0 until VARIANTS) yield Polygon(Seq(makeBlob(-8.0, i * 0.41)))
    val blobsB = for (i <- 0 until VARIANTS) yield Polygon(Seq(makeBlob(8.0, i * 0.67)))
    report(
      "clip_blobs",
      1000,
      i => {
        val a = blobsA((i % VARIANTS).toInt)
        val b = blobsB((i % VARIANTS).toInt)
        PolygonClipping.intersection(a, b).area
      },
    )
  }

  // ---- clipWithHole: a square donut unioned with a square overlapping the hole ----

  def benchClipWithHole(): Unit = {
    val outer = rect(0, 0, 20, 20)
    val hole = PolygonRegion(Seq(V2(8, 8), V2(8, 12), V2(12, 12), V2(12, 8)))
    val donut = Polygon(Seq(outer, hole))
    val clip = Polygon(Seq(rect(9, -5, 11, 11)))
    report("clip_with_hole", 5000, _ => PolygonClipping.union(donut, clip).area)
  }

  def main(args: Array[String]): Unit = {
    println(s"scalaUtils polygon_clipping benchmark - $WARMUP_CHUNKS warmup chunks, then $CHUNKS measured chunks per op\n")
    benchClipSquares()
    benchClipBlobs()
    benchClipWithHole()
  }
}
