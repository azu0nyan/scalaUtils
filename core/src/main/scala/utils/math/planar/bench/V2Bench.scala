package utils.math.planar.bench

import utils.math.planar.V2

/** Manual (no-dependency) micro-benchmark for `V2` ops, meant to be
  * compared against the Rust port's equivalent `math_core`
  * `benches/v2_bench.rs`. Run with:
  *   sbt "core/runMain utils.math.planar.bench.V2Bench"
  *
  * Inputs are drawn from a pre-generated, fixed-size array (cycled via a
  * power-of-two mask) rather than reused constants/fields, so escape
  * analysis / loop-invariant code motion can't hoist the whole loop into a
  * single computation. Warmup runs `WARMUP_CHUNKS` extra, untimed passes
  * through the *exact same* loop shape as the timed `CHUNKS` that follow —
  * not a separately-written warmup loop. That distinction matters a lot on
  * the JVM: the JIT compiles per call-site/loop, so a structurally
  * different warmup loop (even calling the same closure) leaves the timed
  * loop's own call site to warm up *during* the first couple of "measured"
  * chunks, which is exactly what an earlier version of this file showed
  * empirically (`add`/`dot`/`cross` were still 2-3x slower in chunks 1-2
  * before stabilizing at chunk 3, even after a 30,000,000-iteration
  * separately-structured warmup).
  */
object V2Bench {
  val SIZE: Int = 4096
  val MASK: Long = (SIZE - 1).toLong
  val WARMUP_CHUNKS: Long = 3L
  val CHUNKS: Long = 5L
  val ITERS_PER_CHUNK: Long = 10000000L

  def makeInputs(): (Array[V2], Array[V2]) = {
    val a = new Array[V2](SIZE)
    val b = new Array[V2](SIZE)
    for (i <- 0 until SIZE) {
      val t = i.toDouble
      a(i) = V2(Math.sin(t * 0.618) * 10.0, Math.cos(t * 0.381) * 10.0)
      b(i) = V2(Math.cos(t * 0.271) * 5.0 + 1.0, Math.sin(t * 0.141) * 5.0 + 1.0)
    }
    (a, b)
  }

  def benchV2(name: String, a: Array[V2], b: Array[V2])(f: (V2, V2) => V2): Unit = {
    var acc = V2.ZERO
    var c = 0L
    while (c < WARMUP_CHUNKS) {
      val base = c * ITERS_PER_CHUNK
      var j = 0L
      while (j < ITERS_PER_CHUNK) {
        val idx = ((base + j) & MASK).toInt
        acc = acc + f(a(idx), b(idx))
        j += 1
      }
      c += 1
    }

    print(f"$name%-10s")
    acc = V2.ZERO
    var totalNs = 0.0
    var chunk = 0L
    while (chunk < CHUNKS) {
      val base = (WARMUP_CHUNKS + chunk) * ITERS_PER_CHUNK
      val start = System.nanoTime()
      var j = 0L
      while (j < ITERS_PER_CHUNK) {
        val idx = ((base + j) & MASK).toInt
        acc = acc + f(a(idx), b(idx))
        j += 1
      }
      val elapsed = (System.nanoTime() - start).toDouble
      totalNs += elapsed
      print(f" ${elapsed / ITERS_PER_CHUNK}%7.2f")
      chunk += 1
    }
    val avg = totalNs / (CHUNKS * ITERS_PER_CHUNK)
    println(f" ns/op | avg $avg%6.2f   acc=(${acc.x}%.4f, ${acc.y}%.4f)")
  }

  def benchScalar(name: String, a: Array[V2], b: Array[V2])(f: (V2, V2) => Double): Unit = {
    var acc = 0.0
    var c = 0L
    while (c < WARMUP_CHUNKS) {
      val base = c * ITERS_PER_CHUNK
      var j = 0L
      while (j < ITERS_PER_CHUNK) {
        val idx = ((base + j) & MASK).toInt
        acc += f(a(idx), b(idx))
        j += 1
      }
      c += 1
    }

    print(f"$name%-10s")
    acc = 0.0
    var totalNs = 0.0
    var chunk = 0L
    while (chunk < CHUNKS) {
      val base = (WARMUP_CHUNKS + chunk) * ITERS_PER_CHUNK
      val start = System.nanoTime()
      var j = 0L
      while (j < ITERS_PER_CHUNK) {
        val idx = ((base + j) & MASK).toInt
        acc += f(a(idx), b(idx))
        j += 1
      }
      val elapsed = (System.nanoTime() - start).toDouble
      totalNs += elapsed
      print(f" ${elapsed / ITERS_PER_CHUNK}%7.2f")
      chunk += 1
    }
    val avg = totalNs / (CHUNKS * ITERS_PER_CHUNK)
    println(f" ns/op | avg $avg%6.2f   acc=$acc%.4f")
  }

  def main(args: Array[String]): Unit = {
    val (a, b) = makeInputs()
    println(s"scalaUtils V2 benchmark - $WARMUP_CHUNKS warmup chunks x $ITERS_PER_CHUNK iters, then $CHUNKS measured chunks")
    println(f"${"op"}%-10s" + (1 to CHUNKS.toInt).map(c => f" chunk$c%-3d").mkString)

    benchV2("add", a, b)((x, y) => x + y)
    benchV2("sub", a, b)((x, y) => x - y)
    benchScalar("dot", a, b)((x, y) => x ** y)
    benchScalar("cross", a, b)((x, y) => x.cross(y))
    benchScalar("length", a, b)((x, _) => x.length)
    benchV2("normalize", a, b)((x, _) => x.normalize)
    benchV2("rotate", a, b)((x, _) => x.rotate(0.6180339887))
    benchScalar("angle_ccw", a, b)((x, y) => x.angleCCW(y))
  }
}
