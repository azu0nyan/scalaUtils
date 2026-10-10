package utils.math.space.bench

import utils.math.space.{Quat, V3}

/** Micro-benchmarks for `V3`/`Quat` arithmetic hot paths, meant to be
  * compared against the Rust port's equivalent `rust-utils` `benches/space_bench.rs`.
  * Run with:
  *   sbt "core/runMain utils.math.space.bench.SpaceBench"
  *
  * Same `report()`/warmup-then-measured-chunks harness as `GeometryBench` —
  * see that file's doc comment for why the warmup loop has to be
  * structurally identical to the measured one.
  */
object SpaceBench {
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

  val SIZE: Int = 4096
  val MASK: Long = (SIZE - 1).toLong

  def makeV3s(): (Array[V3], Array[V3]) = {
    val a = new Array[V3](SIZE)
    val b = new Array[V3](SIZE)
    for (i <- 0 until SIZE) {
      val t = i.toDouble
      a(i) = V3(Math.sin(t * 0.618) * 10.0, Math.cos(t * 0.381) * 10.0, Math.sin(t * 0.211) * 10.0)
      b(i) = V3(Math.cos(t * 0.271) * 5.0 + 1.0, Math.sin(t * 0.141) * 5.0 + 1.0, Math.cos(t * 0.471) * 5.0 + 1.0)
    }
    (a, b)
  }

  def makeQuats(): (Array[Quat], Array[Quat]) = {
    val a = new Array[Quat](SIZE)
    val b = new Array[Quat](SIZE)
    for (i <- 0 until SIZE) {
      val t = i.toDouble
      a(i) = Quat.fromAngleAxis(t * 0.037, V3(Math.sin(t * 0.1), Math.cos(t * 0.2), 1.0)).q
      b(i) = Quat.fromAngleAxis(t * 0.071 + 1.0, V3(1.0, Math.sin(t * 0.3), Math.cos(t * 0.4))).q
    }
    (a, b)
  }

  def benchV3(a: Array[V3], b: Array[V3]): Unit = {
    report("v3_dot", 10000000, i => a((i & MASK).toInt) ** b((i & MASK).toInt))
    report("v3_cross", 10000000, i => { val c = a((i & MASK).toInt) ^ b((i & MASK).toInt); c.x + c.y + c.z })
    report("v3_normalize", 5000000, i => { val n = a((i & MASK).toInt).normalize; n.x + n.y + n.z })
    report("v3_rotate", 2000000, i => { val r = a((i & MASK).toInt).rotate(b((i & MASK).toInt), 0.6180339887); r.x + r.y + r.z })
  }

  def benchQuat(a: Array[Quat], b: Array[Quat], vs: Array[V3]): Unit = {
    report("quat_mul", 5000000, i => { val q = a((i & MASK).toInt) * b((i & MASK).toInt); q.w + q.x + q.y + q.z })
    report("quat_rotate_vector", 2000000, i => { val v = a((i & MASK).toInt).rotateVector(vs((i & MASK).toInt)); v.x + v.y + v.z })
    report("quat_slerp", 1000000, i => { val q = Quat.slerpNomalized(a((i & MASK).toInt), b((i & MASK).toInt), 0.3); q.w + q.x + q.y + q.z })
  }

  def main(args: Array[String]): Unit = {
    println(s"scalaUtils space benchmark - $WARMUP_CHUNKS warmup chunks, then $CHUNKS measured chunks per op\n")
    val (a, b) = makeV3s()
    benchV3(a, b)
    val (qa, qb) = makeQuats()
    benchQuat(qa, qb, a)
  }
}
