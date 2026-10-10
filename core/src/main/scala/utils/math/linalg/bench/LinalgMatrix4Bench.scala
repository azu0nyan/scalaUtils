package utils.math.linalg.bench

import utils.math.linalg.{Matrix3x3, Matrix4x4}
import utils.math.space.{Quat, Rotations, Transform, V3}

/** The **first** benchmark for `utils.math.linalg` (none existed before
  * this task). Meant to be compared against the Rust port's equivalent
  * `rust-utils` `benches/linalg_bench.rs`.
  * Run with:
  *   sbt "core/runMain utils.math.linalg.bench.LinalgMatrix4Bench"
  *
  * Scala's `Transform` has no working `toMatrix4`/analogous method (the
  * closest thing, a `lazy val matrix: Matrix4x4`, is commented out and
  * dead in `Transform.scala`) — this bench builds the equivalent
  * composition directly via `Matrix4x4.translation`/`.rotationMatrix`/
  * `.scale` instead, matching what the Rust port's `Transform::to_matrix4`
  * actually implements for real.
  */
object LinalgMatrix4Bench {
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

    print(f"$name%-24s")
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

  val SIZE: Int = 2048
  val MASK: Long = (SIZE - 1).toLong

  def makeTransforms(): Array[Transform] = {
    val a = new Array[Transform](SIZE)
    for (i <- 0 until SIZE) {
      val t = i.toDouble
      a(i) = Transform(
        V3(Math.sin(t * 0.1), Math.cos(t * 0.2), Math.sin(t * 0.3)),
        Quat.fromAngleAxis(t * 0.037, V3(1.0, Math.sin(t * 0.4), Math.cos(t * 0.5))),
        V3(1.0 + Math.sin(t * 0.05) * 0.5, 1.0, 1.0),
      )
    }
    a
  }

  def toMatrix4(t: Transform): Matrix4x4 =
    Matrix4x4.translation(t.translation) * Matrix4x4.rotationMatrix(t.rotation) * Matrix4x4.scale(t.scale)

  def benchMatrix4TransformPoint(transforms: Array[Transform]): Unit = {
    val matrices = transforms.map(toMatrix4)
    val p = V3(1.0, 2.0, 3.0)
    report(
      "matrix4_transform_point",
      2000000,
      i => {
        val r = matrices((i & MASK).toInt).homoTransformPoint(p)
        r.x + r.y + r.z
      },
    )
  }

  def rotationZ(angle: Double): Matrix3x3 = {
    val s = Math.sin(angle)
    val c = Math.cos(angle)
    Matrix3x3(c, -s, 0.0, s, c, 0.0, 0.0, 0.0, 1.0)
  }

  def benchMatrix3MulV3(): Unit = {
    val Q_SIZE = 2048
    val mats = (0 until Q_SIZE).map(i => rotationZ(i.toDouble)).toArray
    val v = V3(1.0, 2.0, 3.0)
    report(
      "matrix3_mul_v3",
      5000000,
      i => {
        val r = mats((i & (Q_SIZE - 1).toLong).toInt) * v
        r.x + r.y + r.z
      },
    )
  }

  def benchTransformToMatrix4(transforms: Array[Transform]): Unit = {
    report(
      "transform_to_matrix4",
      500000,
      i => {
        val m = toMatrix4(transforms((i & MASK).toInt))
        m.m00 + m.m11 + m.m22 + m.m33
      },
    )
  }

  def main(args: Array[String]): Unit = {
    println(s"scalaUtils linalg benchmark - $WARMUP_CHUNKS warmup chunks, then $CHUNKS measured chunks per op\n")
    val transforms = makeTransforms()
    benchMatrix4TransformPoint(transforms)
    benchMatrix3MulV3()
    benchTransformToMatrix4(transforms)
  }
}
