package utils.mesh.bench

import utils.math.space.{Sphere, V3}
import utils.mesh.{MutableMeshSection, factory}
import utils.mesh.obj.ObjReader

/** Micro-benchmarks for `utils.mesh`, meant to be compared against the
  * Rust port's equivalent `rust-mesh` `benches/mesh_bench.rs`. `utils.mesh`
  * had zero benchmarks before this port; this file exists purely so the
  * Rust side still has a Scala baseline, same as every other crate in this
  * repo. Run with:
  *   sbt "core/runMain utils.mesh.bench.MeshBench"
  */
object MeshBench {
  val WARMUP_CHUNKS: Long = 15L
  val CHUNKS: Long = 5L

  def report(name: String, warmupChunks: Long, itersPerChunk: Long, step: Long => Double): Unit = {
    var acc = 0.0
    var c = 0L
    while (c < warmupChunks) {
      val base = c * itersPerChunk
      var j = 0L
      while (j < itersPerChunk) {
        acc += step(base + j)
        j += 1
      }
      c += 1
    }

    print(f"$name%-32s")
    acc = 0.0
    var totalNs = 0.0
    var chunk = 0L
    while (chunk < CHUNKS) {
      val base = (warmupChunks + chunk) * itersPerChunk
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

  def benchSphereConstruction(): Unit = {
    val s = Sphere(V3.ZERO, 5.0)
    report(
      "sphere_construction (64x64)",
      15L,
      200L,
      _ => factory.Sphere(s, 64, 64).vertices.size.toDouble,
    )
  }

  def benchAddOtherMerge(): Unit = {
    val small = factory.Sphere(Sphere(V3.ZERO, 1.0), 8, 8)
    report(
      "mesh_add_other_merge (20x)",
      15L,
      2000L,
      _ => {
        val acc = new MutableMeshSection()
        var i = 0
        while (i < 20) {
          acc ++= small
          i += 1
        }
        acc.vertices.size.toDouble
      },
    )
  }

  def benchSplitByParts(): Unit = {
    val big = factory.Sphere(Sphere(V3.ZERO, 5.0), 64, 64)
    report(
      "split_by_parts (64x64 sphere)",
      10L,
      200L,
      _ => big.splitByParts(500).size.toDouble,
    )
  }

  /** Same deliberately-unoptimized (no shared vertices), valid OBJ text
    * shape as the Rust bench's `make_obj_text`.
    */
  def makeObjText(triangleCount: Int): String = {
    val sb = new StringBuilder("o Mesh\n")
    var i = 0
    while (i < triangleCount) {
      val x = i.toDouble
      sb.append(s"v $x 0 0\nv ${x + 0.1} 1 0\nv ${x + 0.2} 0 1\n")
      sb.append("vt 0 0\nvt 1 0\nvt 0 1\n")
      i += 1
    }
    sb.append("vn 0 0 1\n")
    i = 0
    while (i < triangleCount) {
      val base = i * 3 + 1
      sb.append(s"f $base/$base/1 ${base + 1}/${base + 1}/1 ${base + 2}/${base + 2}/1\n")
      i += 1
    }
    sb.toString()
  }

  def benchObjReadFrom(): Unit = {
    val text = makeObjText(3000)
    report(
      "obj_read_from (3000 triangles)",
      10L,
      50L,
      _ => ObjReader.readFrom(text.linesIterator)(true, false).vertices.size.toDouble,
    )
  }

  def main(args: Array[String]): Unit = {
    println(s"scalaUtils mesh benchmark - $WARMUP_CHUNKS warmup chunks, then $CHUNKS measured chunks per op\n")
    benchSphereConstruction()
    benchAddOtherMerge()
    benchSplitByParts()
    benchObjReadFrom()
  }
}
