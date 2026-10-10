package utils.math.planar.bench

import utils.math.planar.algo.{ConvexHull, PolygonContains}
import utils.math.planar.patch.BezierCurve
import utils.math.planar.{PointIntersection, PolygonRegion, SegmentPlanar, V2}

import java.util.logging.{Level, Logger}

/** Micro-benchmarks for the higher-level algorithms, meant to be compared
  * against the Rust port's equivalent `math_core` `benches/geometry_bench.rs`.
  * Run with:
  *   sbt "core/runMain utils.math.planar.bench.GeometryBench"
  *
  * Each benchmark reports timing in `CHUNKS` sequential slices, preceded by
  * `WARMUP_CHUNKS` extra untimed passes through the *exact same* loop shape
  * (not a separately-written warmup loop) — see `V2Bench`'s doc comment for
  * why that distinction matters on the JVM.
  *
  * `polygon_contains` sets the `"UTILS"` `java.util.logging` logger to
  * `OFF` before running. `PolygonContains.contains` calls
  * `Logging.logger.info(s"...")` at nearly every sweep-line step — even
  * with the logger level at `OFF`, the `s"..."` *argument* (which maps/
  * formats the whole sweep structure) is still built eagerly before the
  * (now-suppressed) log call, so this does not fully eliminate that cost.
  * Measured: ~1.07ms/call with default logging, ~175us/call with the
  * logger at `OFF` — vs. ~1.2us/call for the Rust port, which drops the
  * logging entirely. That ~150x gap (even past the logger-level fix) is
  * the eager-string-interpolation cost alone; see `docs/porting.md` in
  * `math-core` for the full writeup.
  */
object GeometryBench {
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

  // ---- ConvexHull: 256-point wavy blob ----
  val HULL_POINTS: Int = 256

  def makeBlob(seedOffset: Double): IndexedSeq[V2] =
    for (i <- 0 until HULL_POINTS) yield {
      val t = (i.toDouble + seedOffset) / HULL_POINTS * 2 * Math.PI
      val r = 20.0 + 6.0 * Math.sin(7.0 * t)
      V2(r * Math.cos(t), r * Math.sin(t))
    }

  def benchConvexHull(): Unit = {
    val BLOB_VARIANTS = 64
    val blobs = for (i <- 0 until BLOB_VARIANTS) yield makeBlob(i * 0.37)
    report(
      "convex_hull",
      1000,
      i => ConvexHull.convexHull(blobs((i % BLOB_VARIANTS).toInt)).map(_.vertices.size.toDouble).getOrElse(0.0),
    )
  }

  // ---- PolygonContains: rectangle parent/child, ported from the "Simple Contains" fixture ----
  def benchPolygonContains(): Unit = {
    // See the class doc comment: logger level alone doesn't fully remove
    // the cost (eager `s"..."` argument construction), but it's the most
    // a caller can do without editing the library, and it does help.
    Logger.getLogger("UTILS").setLevel(Level.OFF)
    val parent = Seq(Seq(V2(-10.0, -10.0), V2(10.0, -10.0), V2(10.0, 10.0), V2(-10.0, 10.0)))
    val child = Seq(Seq(V2(-5.0, -5.0), V2(5.0, -5.0), V2(5.0, 5.0), V2(-5.0, 5.0)))
    report(
      "polygon_contains",
      2000,
      _ => if (PolygonContains.contains(parent, child, false)) 1.0 else 0.0,
    )
  }

  // ---- PolygonRegion.classify: the "harder 2" concave fixture, cycling query points ----
  def benchClassify(): Unit = {
    val region = PolygonRegion(Seq(
      V2(1.0, 0.0), V2(2.0, 0.0), V2(2.0, 1.0), V2(3.0, 1.0), V2(3.0, 3.0),
      V2(2.0, 3.0), V2(2.0, 2.0), V2(1.0, 2.0), V2(1.0, 3.0), V2(0.0, 3.0),
      V2(0.0, 1.0), V2(1.0, 1.0),
    ))
    val QUERY_COUNT = 1024
    val queries = for (i <- 0 until QUERY_COUNT) yield {
      val t = i.toDouble
      V2(Math.sin(t * 0.618) * 2.0 + 1.5, Math.cos(t * 0.381) * 2.0 + 1.5)
    }
    report(
      "classify",
      200000,
      i =>
        region.classify(queries((i % QUERY_COUNT).toInt)) match {
          case PolygonRegion.INSIDE => 1.0
          case PolygonRegion.BORDER => 0.5
          case _ => 0.0
        },
    )
  }

  // ---- SegmentPlanar.intersection, cycling segment pairs ----
  val SEG_SIZE: Int = 4096
  val SEG_MASK: Long = (SEG_SIZE - 1).toLong

  def makeSegments(): (Array[SegmentPlanar], Array[SegmentPlanar]) = {
    val a = new Array[SegmentPlanar](SEG_SIZE)
    val b = new Array[SegmentPlanar](SEG_SIZE)
    for (i <- 0 until SEG_SIZE) {
      val t = i.toDouble
      a(i) = SegmentPlanar(
        V2(Math.sin(t * 0.618) * 10.0, Math.cos(t * 0.381) * 10.0),
        V2(Math.cos(t * 0.512) * 10.0, Math.sin(t * 0.291) * 10.0),
      )
      b(i) = SegmentPlanar(
        V2(Math.cos(t * 0.271) * 10.0 + 1.0, Math.sin(t * 0.141) * 10.0 + 1.0),
        V2(Math.sin(t * 0.071) * 10.0 - 1.0, Math.cos(t * 0.341) * 10.0 - 1.0),
      )
    }
    (a, b)
  }

  def benchSegmentIntersection(): Unit = {
    val (a, b) = makeSegments()
    report(
      "seg_intersect",
      2000000,
      i =>
        a((i & SEG_MASK).toInt).intersection(b((i & SEG_MASK).toInt)) match {
          case Some(PointIntersection(p)) => p.x + p.y
          case _ => 0.0
        },
    )
  }

  // ---- Cubic Bezier evaluation, cycling t ----
  def benchBezierEval(): Unit = {
    val curve = BezierCurve(Seq(V2(0.0, 0.0), V2(3.0, 10.0), V2(8.0, 2.0), V2(10.0, 8.0)))
    val T_SIZE = 4096
    val ts = for (i <- 0 until T_SIZE) yield i.toDouble / T_SIZE
    report(
      "bezier_eval",
      5000000,
      i => {
        val p = curve(ts((i % T_SIZE).toInt))
        p.x + p.y
      },
    )
  }

  def main(args: Array[String]): Unit = {
    println(s"scalaUtils geometry benchmark - $WARMUP_CHUNKS warmup chunks, then $CHUNKS measured chunks per op\n")
    benchConvexHull()
    benchPolygonContains()
    benchClassify()
    benchSegmentIntersection()
    benchBezierEval()
  }
}
