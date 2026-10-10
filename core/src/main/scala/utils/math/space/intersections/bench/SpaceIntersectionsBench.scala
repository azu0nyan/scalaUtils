package utils.math.space.intersections.bench

import utils.datastructures.spatial.TriangleSoup
import utils.math.space.intersections.{Intersects, SegmentCast, TriangleToTriangle}
import utils.datastructures.spatial.TriangleSoup.storage
import utils.math.space.{AABox, Ray, Segment, Sphere, Triangle, UnitV3, V3}

/** Micro-benchmarks for `utils.math.space.intersections`, meant to be
  * compared against the Rust port's equivalent `rust-utils`
  * `benches/space_intersections_bench.rs`.
  * Run with:
  *   sbt "core/runMain utils.math.space.intersections.bench.SpaceIntersectionsBench"
  */
object SpaceIntersectionsBench {
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

  val SIZE: Int = 2048
  val MASK: Long = (SIZE - 1).toLong

  def makeRays(): Array[Ray] = {
    val a = new Array[Ray](SIZE)
    for (i <- 0 until SIZE) {
      val t = i.toDouble
      val origin = V3(Math.sin(t * 0.618) * 20.0, Math.cos(t * 0.381) * 20.0, Math.sin(t * 0.211) * 20.0)
      val dir = new UnitV3(V3(Math.cos(t * 0.271), Math.sin(t * 0.141), Math.cos(t * 0.471) + 0.1).normalize)
      a(i) = Ray(origin, dir)
    }
    a
  }

  def makeSegments(): Array[Segment] = {
    val a = new Array[Segment](SIZE)
    for (i <- 0 until SIZE) {
      val t = i.toDouble
      val start = V3(Math.sin(t * 0.618) * 20.0, Math.cos(t * 0.381) * 20.0, Math.sin(t * 0.211) * 20.0)
      val end = V3(Math.cos(t * 0.271) * 20.0, Math.sin(t * 0.141) * 20.0, Math.cos(t * 0.471) * 20.0)
      a(i) = Segment(start, end)
    }
    a
  }

  def benchRayToAABox(rays: Array[Ray]): Unit = {
    val b = AABox(V3(-1.0), V3(1.0))
    report("ray_to_aabb", 2000000, i => if (Intersects.rayToAABox(rays((i & MASK).toInt), b)) 1.0 else 0.0)
  }

  def benchRayToSphere(rays: Array[Ray]): Unit = {
    val s = Sphere(V3.ZERO, 1.0)
    report("ray_to_sphere", 2000000, i => if (Intersects.rayToSphere(rays((i & MASK).toInt), s)) 1.0 else 0.0)
  }

  def benchRayToTriangle(rays: Array[Ray]): Unit = {
    val tri = Triangle(V3(-1.0, -1.0, 0.0), V3(1.0, -1.0, 0.0), V3(0.0, 1.0, 0.0))
    report("ray_to_triangle", 1000000, i => if (Intersects.rayToTriangle(rays((i & MASK).toInt), tri)) 1.0 else 0.0)
  }

  def benchTriangleToTriangle(): Unit = {
    val T_SIZE = 512
    val tris = for (i <- 0 until T_SIZE) yield {
      val t = i.toDouble
      val off = V3(Math.sin(t * 0.3) * 2.0, Math.cos(t * 0.4) * 2.0, Math.sin(t * 0.5) * 2.0)
      Triangle(off, off + V3(1.0, 0.0, 0.0), off + V3(0.0, 1.0, 0.1))
    }
    val fixed = Triangle(V3(-1.0, -1.0, -1.0), V3(1.0, -1.0, 1.0), V3(0.0, 1.0, 0.0))
    report(
      "triangle_to_triangle",
      1000000,
      i => {
        val t2 = tris(i.toInt % T_SIZE)
        if (TriangleToTriangle.apply(fixed.a, fixed.b, fixed.c, t2.a, t2.b, t2.c)) 1.0 else 0.0
      },
    )
  }

  def benchSegmentCastAABox(segs: Array[Segment]): Unit = {
    val b = AABox(V3(-1.0), V3(1.0))
    report(
      "segment_cast_aabb",
      1000000,
      i =>
        SegmentCast.toAABox(segs((i & MASK).toInt), b) match {
          case Some((p, _)) => p.x + p.y + p.z
          case None => 0.0
        },
    )
  }

  def benchTriangleSoupQuery(): Unit = {
    val N = 500
    val triangles = for (i <- 0 until N) yield {
      val off = V3(i * 0.5, 0.0, 0.0)
      Triangle(off, off + V3(1.0, 0.0, 0.0), off + V3(0.0, 1.0, 0.0))
    }
    val soup = TriangleSoup(triangles)
    val Q_SIZE = 256
    val queries = for (i <- 0 until Q_SIZE) yield {
      val c = V3(i * 1.0, 0.0, 0.0)
      AABox(c - V3(2.0), c + V3(2.0))
    }
    report(
      "triangle_soup_query (naive, 500 tris)",
      50000,
      i => soup.data.values(queries(i.toInt % Q_SIZE)).size.toDouble,
    )
  }

  def main(args: Array[String]): Unit = {
    println(s"scalaUtils space_intersections benchmark - $WARMUP_CHUNKS warmup chunks, then $CHUNKS measured chunks per op\n")
    val rays = makeRays()
    val segs = makeSegments()
    benchRayToAABox(rays)
    benchRayToSphere(rays)
    benchRayToTriangle(rays)
    benchTriangleToTriangle()
    benchSegmentCastAABox(segs)
    benchTriangleSoupQuery()
  }
}
