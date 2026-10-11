package utils.math.planar.algo.bench

import utils.math.planar.V2
import utils.math.planar.algo.{PolygonToConvex, PolygonTriangulation}

/** Micro-benchmark for `PolygonTriangulation` / `PolygonToConvex` (monotone
  * partition, monotone triangulation and convex decomposition on a DCEL),
  * meant to be compared against the Rust port's
  * `rust_utils/benches/monotone_triangulation_bench.rs`. Run with:
  *   sbt "triangulation/runMain utils.math.planar.algo.bench.PolygonTriangulationBench"
  *
  * Same methodology as `EarcutBench`: `WARMUP_CHUNKS` untimed passes through
  * the exact same loop shape as the `CHUNKS` timed passes that follow.
  * Iteration counts are far lower than `EarcutBench`'s: every call builds
  * and cuts a DCEL, which is roughly quadratic per ring. They are the same
  * on both sides.
  *
  * Accumulators are counts (triangles / monotone faces / convex pieces),
  * not geometry: the two implementations may pick different diagonals. A
  * call that throws contributes `-1`.
  */
object PolygonTriangulationBench {
  val WARMUP_CHUNKS: Long = 5L
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
      print(f" ${elapsed / itersPerChunk}%11.1f")
      chunk += 1
    }
    val avg = totalNs / (CHUNKS * itersPerChunk)
    println(f"  | avg $avg%11.1f ns/op   (acc=$acc%.3f)")
  }

  // ---- inputs ----

  /** The wavy blob of `EarcutBench` with a variable point count. */
  def makeBlob(points: Int, seedOffset: Double): IndexedSeq[V2] =
    for (i <- 0 until points) yield {
      val t = (i.toDouble + seedOffset) / points * 2 * Math.PI
      val r = 20.0 + 6.0 * Math.sin(7.0 * t)
      V2(r * Math.cos(t), r * Math.sin(t))
    }

  val BLOB_VARIANTS: Int = 16

  def blobs(points: Int): IndexedSeq[Seq[Seq[V2]]] =
    for (i <- 0 until BLOB_VARIANTS) yield Seq(makeBlob(points, i * 0.37))

  /** The square donut of `EarcutBench`: outer ring and one square hole. */
  val donut: Seq[Seq[V2]] = Seq(
    Seq(V2(-20.0, -20.0), V2(20.0, -20.0), V2(20.0, 20.0), V2(-20.0, 20.0)),
    Seq(V2(-10.0, -10.0), V2(10.0, -10.0), V2(10.0, 10.0), V2(-10.0, 10.0)),
  )

  /** A comb with `teeth` teeth pointing up; every gap bottom is a merge
    * vertex. Heights differ slightly so that no two events tie.
    */
  def makeComb(teeth: Int): Seq[V2] = {
    val ring = scala.collection.mutable.ArrayBuffer(V2(0.0, 0.0), V2(teeth * 2.0, 0.0))
    for (i <- (0 until teeth).reverse) {
      val x = i * 2.0
      val h = 10.0 + i * 0.125
      ring += V2(x + 1.5, h)
      ring += V2(x + 0.5, h + 0.0625)
      if (i > 0) ring += V2(x - 0.25, 2.0 + i * 0.03125)
    }
    ring.toSeq
  }

  val COMB_TEETH: Int = 16

  // ---- iteration counts (shared with the Rust bench) ----
  val ITERS_BLOB64: Long = 1000L
  val ITERS_BLOB256: Long = 100L
  val ITERS_DONUT: Long = 10000L
  val ITERS_COMB: Long = 1000L
  val ITERS_TO_CONVEX: Long = 400L

  var failures: Map[String, String] = Map()

  /** Result count, or -1 if the call throws (the first message per case is kept). */
  def count(name: String)(body: => Int): Double =
    try body.toDouble
    catch {
      case e: Throwable =>
        if (!failures.contains(name)) failures += name -> e.toString
        -1.0
    }

  def benchTriangulate(): Unit = {
    val blobs64 = blobs(64)
    report("tri_blob64", ITERS_BLOB64,
      i => count("tri_blob64")(PolygonTriangulation.triangulateNonHoles(blobs64((i % BLOB_VARIANTS).toInt)).size))
    val blobs256 = blobs(256)
    report("tri_blob256", ITERS_BLOB256,
      i => count("tri_blob256")(PolygonTriangulation.triangulateNonHoles(blobs256((i % BLOB_VARIANTS).toInt)).size))
    report("tri_donut", ITERS_DONUT, _ => count("tri_donut")(PolygonTriangulation.triangulateNonHoles(donut).size))
    val comb = Seq(makeComb(COMB_TEETH))
    report("tri_comb", ITERS_COMB, _ => count("tri_comb")(PolygonTriangulation.triangulateNonHoles(comb).size))
  }

  def benchMonotonePartition(): Unit = {
    val blobs64 = blobs(64)
    report("monotone_blob64", ITERS_BLOB64,
      i => count("monotone_blob64")(PolygonTriangulation.monotonePartitionNonHole(blobs64((i % BLOB_VARIANTS).toInt)).size))
  }

  def benchToConvex(): Unit = {
    val blobs64 = blobs(64)
    report("to_convex_blob64", ITERS_TO_CONVEX,
      i => count("to_convex_blob64")(PolygonToConvex.toConvexPolygons(blobs64((i % BLOB_VARIANTS).toInt)).size))
  }

  /** Per-variant result counts of the 64-point blob cases (run with the
    * argument `counts`), for comparing accumulators with the Rust port.
    */
  def printCounts(): Unit = {
    val blobs64 = blobs(64)
    println("monotone  " + blobs64.map(b => count("monotone")(PolygonTriangulation.monotonePartitionNonHole(b).size).toInt).mkString(" "))
    println("triangles " + blobs64.map(b => count("triangles")(PolygonTriangulation.triangulateNonHoles(b).size).toInt).mkString(" "))
    println("to_convex " + blobs64.map(b => count("to_convex")(PolygonToConvex.toConvexPolygons(b).size).toInt).mkString(" "))
  }

  def main(args: Array[String]): Unit = {
    if (args.contains("counts")) {
      printCounts()
      return
    }
    println(s"scalaUtils monotone triangulation benchmark - $WARMUP_CHUNKS warmup chunks, then $CHUNKS measured chunks per op\n")
    benchTriangulate()
    benchMonotonePartition()
    benchToConvex()
    for ((name, error) <- failures.toSeq.sortBy(_._1)) println(s"FAILED $name: $error")
  }
}
