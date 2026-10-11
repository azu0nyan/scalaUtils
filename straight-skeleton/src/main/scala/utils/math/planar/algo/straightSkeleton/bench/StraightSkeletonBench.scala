package utils.math.planar.algo.straightSkeleton.bench

import utils.math.planar.algo.straightSkeleton.helpers.SSOps
import utils.math.planar.algo.straightSkeleton.implhelpers.{Loop, LoopL}
import utils.math.planar.algo.straightSkeleton.math.LinearForm3D
import utils.math.planar.algo.straightSkeleton.{Corner, Edge, Machine, OffsetSkeleton}
import utils.math.space.V3

/** Micro-benchmark for the `straightSkeleton` package (the Scala port of
  * Tom Kelly's campskeleton), meant to be compared against the Rust port's
  * equivalent `rust_utils` `benches/straight_skeleton_bench.rs`. Run with:
  *   sbt "straight-skeleton/runMain utils.math.planar.algo.straightSkeleton.bench.StraightSkeletonBench"
  *
  * Same methodology as `EarcutBench`: `WARMUP_CHUNKS` untimed passes through
  * the exact same loop shape as the `CHUNKS` timed passes that follow.
  *
  * Passing the single argument `dump` skips the timing and instead prints
  * every case's output (faces / offset loops, `%.6f` coordinates) once, in
  * the same text format the Rust bench prints for `-- dump`, so the two
  * ports can be diffed case by case.
  *
  * Inputs that go through `sin`/`cos` are rounded to 1e-6 (`snap`) so both
  * languages feed bit-identical polygons to the algorithm.
  */
object StraightSkeletonBench {
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

    print(f"$name%-22s")
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

  // ---- inputs -------------------------------------------------------------

  type Poly = Seq[(Double, Double)]

  def snap(x: Double): Double = Math.floor(x * 1e6 + 0.5) / 1e6

  val square: Poly = Seq((100.0, 100.0), (-100.0, 100.0), (-100.0, -100.0), (100.0, -100.0))

  /** Regular CCW n-gon of radius 100, coordinates snapped to 1e-6. */
  def ngon(n: Int): Poly =
    for (i <- 0 until n) yield {
      val t = i.toDouble / n * 2 * Math.PI
      (snap(100.0 * Math.cos(t)), snap(100.0 * Math.sin(t)))
    }

  /** 32-gon whose radius wobbles deterministically between 85 and 115. */
  def jittered(n: Int): Poly =
    for (i <- 0 until n) yield {
      val t = i.toDouble / n * 2 * Math.PI
      val r = 100.0 + 15.0 * Math.sin(i * 2.3)
      (snap(r * Math.cos(t)), snap(r * Math.sin(t)))
    }

  /** A comb: a 20-high base strip with `teeth` teeth (pitch 30, width 12, height 40/47/54 cycling) on top. CCW. */
  def comb(teeth: Int): Poly = {
    val w = teeth * 30.0
    val b = Seq.newBuilder[(Double, Double)]
    b += ((0.0, 0.0))
    b += ((w, 0.0))
    b += ((w, 20.0))
    var i = teeth - 1
    while (i >= 0) {
      val h = 40.0 + 7.0 * (i % 3)
      val x0 = 30.0 * i + 9.0
      val x1 = 30.0 * i + 21.0
      b += ((x1, 20.0))
      b += ((x1, 20.0 + h))
      b += ((x0, 20.0 + h))
      b += ((x0, 20.0))
      i -= 1
    }
    b += ((0.0, 20.0))
    b.result()
  }

  val bug1: Poly = Seq((500.0, 0.0), (500.0, 100.0), (400.0, 100.0), (400.0, 200.0), (300.0, 200.0), (300.0, 0.0), (500.0, -100.0))
  val bug2: Poly = Seq((600.0, -100.0), (600.0, 0.0), (500.0, 0.0), (500.0, 400.0), (400.0, 400.0), (400.0, -100.0), (500.0, -200.0))

  /** `debugDrawing/HardCase.vs` (test sources are not on the main classpath, so it is repeated here). */
  val hardCase: Poly = Seq(
    (-600.0, -500.0), (700.0, -500.0), (700.0, 400.0), (-600.0, 400.0), (-600.0, -300.0), (500.0, -300.0), (500.0, 200.0), (-400.0, 200.0),
    (-400.0, -100.0), (400.0, -100.0), (300.0, 0.0), (-200.0, 0.0), (-300.0, 0.0), (-300.0, 100.0), (400.0, 100.0), (436.0, -106.0),
    (400.0, -200.0), (-500.0, -200.0), (-500.0, 300.0), (600.0, 300.0), (600.0, -400.0), (-600.0, -400.0), (-695.0, -267.0), (-667.0, -249.0),
    (-658.0, -144.0), (-691.0, -87.0), (-767.0, -94.0), (-785.0, -115.0), (-708.0, -136.0), (-699.0, -188.0), (-772.0, -203.0), (-823.0, -138.0),
    (-824.0, -59.0), (-790.0, -6.0), (-743.0, 32.0), (-685.0, 55.0), (-678.0, 181.0), (-682.0, 276.0), (-796.0, 419.0), (-867.0, 413.0),
    (-902.0, 344.0), (-940.0, 189.0), (-939.0, 106.0), (-924.0, -48.0), (-910.0, -222.0), (-910.0, -312.0), (-926.0, -510.0), (-878.0, -494.0),
    (-883.0, -433.0), (-873.0, -404.0), (-837.0, -373.0), (-816.0, -412.0), (-820.0, -454.0), (-822.0, -497.0), (-751.0, -477.0), (-705.0, -439.0),
    (-685.0, -446.0), (-659.0, -439.0), (-628.0, -450.0), (-596.0, -464.0), (-610.0, -497.0), (-655.0, -498.0), (-624.0, -524.0), (-595.0, -524.0),
  )

  val triangle: Poly = Seq((0.0, 0.0), (100.0, 0.0), (100.0, 100.0))

  // ---- measured operations ------------------------------------------------

  def faces(p: Poly): Seq[Seq[V3]] = SSOps.dumpFaces(SSOps.calculateFor(p*))

  /** Order-independent accumulator: face count + total face vertex count. */
  def skeletonAcc(p: Poly): Double = {
    val fs = faces(p)
    (fs.size + fs.map(_.size).sum).toDouble
  }

  def shrinkLoops(p: Poly, dist: Double): Seq[Seq[V3]] = {
    val corners = p.map((x, y) => new Corner(x, y))
    val machine = new Machine()
    val loop = new Loop[Edge]()
    for (i <- corners.indices) {
      val e = new Edge(corners(i), corners((i + 1) % corners.size))
      e.machine = machine
      loop.append(e)
    }
    val out = OffsetSkeleton.shrink(new LoopL[Edge](loop), dist)
    out.iterator.map(_.iterator.toSeq.map(_.asInstanceOf[Corner].asV3)).toSeq
  }

  def planArea(l: Seq[V3]): Double = {
    var a = 0.0
    for (i <- l.indices) {
      val p = l(i)
      val q = l((i + 1) % l.size)
      a += p.x * q.y - q.x * p.y
    }
    Math.abs(a) / 2
  }

  /** Order-independent accumulator: corner count + plan area of every offset loop. */
  def shrinkAcc(p: Poly, dist: Double): Double = {
    val ls = shrinkLoops(p, dist)
    ls.map(l => l.size + planArea(l)).sum
  }

  def collideAcc(i: Long): Double = {
    val d = (i % 64).toDouble
    val a = new LinearForm3D(0.7071067811865476, -0.0, 0.7071067811865475, -353.5533905932738 - d)
    val b = new LinearForm3D(-0.5000000000000001, -0.5000000000000001, 0.7071067811865476, 150.00000000000006 + d)
    val c = new LinearForm3D(0.5000000000000001, -0.5000000000000001, 0.7071067811865476, -350.0000000000001)
    a.collide(b, c) match
      case Some(v) => v.x + v.y + v.z
      case None => 0.0
  }

  val skeletonCases: Seq[(String, Poly, Long)] = Seq(
    ("square", square, 8000L),
    ("ngon_8", ngon(8), 1500L),
    ("ngon_32", ngon(32), 150L),
    ("ngon_128", ngon(128), 6L),
    ("jitter_32", jittered(32), 60L),
    ("comb_4", comb(4), 600L),
    ("comb_16", comb(16), 60L),
    ("comb_64", comb(64), 4L),
    ("bug_1", bug1, 2500L),
    ("bug_2", bug2, 1500L),
    ("hard_case", hardCase, 10L),
  )

  val shrinkCases: Seq[(String, Poly, Double, Long)] = Seq(
    ("shrink_triangle", triangle, 5.0, 8000L),
    ("shrink_ngon_32", ngon(32), 5.0, 150L),
  )

  def dumpLoops(name: String, loops: Seq[Seq[V3]]): Unit = {
    println(s"== $name: ${loops.size} loops")
    for (l <- loops)
      println(l.map(v => f"(${v.x + 0.0}%.6f, ${v.y + 0.0}%.6f, ${v.z + 0.0}%.6f)").mkString(" "))
  }

  def main(args: Array[String]): Unit = {
    if (args.contains("dump")) {
      for ((name, poly, _) <- skeletonCases) dumpLoops(name, faces(poly))
      for ((name, poly, dist, _) <- shrinkCases) dumpLoops(name, shrinkLoops(poly, dist))
    } else {
      println(s"scalaUtils straight skeleton benchmark - $WARMUP_CHUNKS warmup chunks, then $CHUNKS measured chunks per op\n")
      for ((name, poly, iters) <- skeletonCases) report(name, iters, _ => skeletonAcc(poly))
      for ((name, poly, dist, iters) <- shrinkCases) report(name, iters, _ => shrinkAcc(poly, dist))
      report("collide_3planes", 200000L, collideAcc)
    }
  }
}
