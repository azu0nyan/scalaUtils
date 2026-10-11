package utils.datastructures.bench

import utils.datastructures.containers.{BinHeap, DisjointSet}
import utils.datastructures.containers.ThreadedAVLTree.ThreadedAVLTree
import utils.datastructures.graph.{ArrayBufferGraph, Graph}
import utils.datastructures.spatial.SpatialStorage.NaiveSpatialStorageImplementation
import utils.datastructures.spatial.Octree.OctreeStorage
import utils.datastructures.IntV2
import utils.heightmap.{HeightGridRandom, Pow2ArrayHeightGrid, TiledHeightmap}
import utils.math.planar.V2
import utils.math.space.{AABox, V3}

/** Micro-benchmarks for `utils.datastructures` (containers, graph, and the
  * Octree-vs-Naive spatial-index comparison), meant to be compared against
  * the Rust port's equivalent `rust-utils` `benches/datastructures_bench.rs`.
  * Run with:
  *   sbt "core/runMain utils.datastructures.bench.DataStructuresBench"
  */
object DataStructuresBench {
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

    print(f"$name%-28s")
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

  def benchBinHeapAddPoll(): Unit = {
    val heap = new BinHeap[Long]()
    var i = 0L
    while (i < 256L) {
      heap.add(i * 97L % 100000L)
      i += 1
    }
    report(
      "bin_heap_add_poll",
      1000000L,
      i => {
        val key = ((i * 1000003L) % 10000000L) + 200000L
        heap.add(key)
        heap.poll().toDouble
      },
    )
  }

  def benchDisjointSetUnionFind(): Unit = {
    val N = 4096L
    val ds = new DisjointSet[Long]()
    var i = 0L
    while (i < N) {
      ds.makeSet(i)
      i += 1
    }
    report(
      "disjoint_set_union_find",
      1000000L,
      i => {
        val a = (i * 1000003L) % N
        val b = (i * 999983L) % N
        ds.union(a, b)
        ds.findSet(a).toDouble
      },
    )
  }

  def benchAvlTreeAddContains(): Unit = {
    val tree = new ThreadedAVLTree[Long]()
    var i = 0L
    while (i < 2048L) {
      tree.add(i * 97L % 1000000L)
      i += 1
    }
    report(
      "avl_tree_add_contains",
      500000L,
      i => {
        val v = (i * 1000003L) % 1000000L
        if (tree.contains(v)) 1.0 else 0.0
      },
    )
  }

  def makeGridGraph(): ArrayBufferGraph[Long, Double] = {
    val W = 40L
    val H = 40L
    val g = new ArrayBufferGraph[Long, Double]()
    def id(x: Long, y: Long): Long = y * W + x
    var y = 0L
    while (y < H) {
      var x = 0L
      while (x < W) {
        val n = id(x, y)
        if (x + 1 < W) {
          g.addEdge(n, id(x + 1, y), 1.0)
          g.addEdge(id(x + 1, y), n, 1.0)
        }
        if (y + 1 < H) {
          g.addEdge(n, id(x, y + 1), 1.0)
          g.addEdge(id(x, y + 1), n, 1.0)
        }
        x += 1
      }
      y += 1
    }
    g
  }

  def benchGraphShortestPath(): Unit = {
    val W = 40L
    val g = makeGridGraph()
    report(
      "graph_shortest_path (A*)",
      2000L,
      i => {
        val start = (i * 37L) % (W * W)
        val end = (W * W - 1L) - start
        g.shortestPath(start, end, (e: Double) => e, (_: Long) => 0d) match {
          case Some(p) => p.length(e => e)
          case None => -1.0
        }
      },
    )
  }

  def benchOctreeQueryVsNaive(): Unit = {
    val N = 500
    val aabbs = for (i <- 0 until N) yield {
      val off = V3(i * 0.5, 0.0, 0.0)
      AABox(off, off + V3(0.5, 1.0, 1.0))
    }

    val naive = new NaiveSpatialStorageImplementation[AABox, Int]()
    for ((b, idx) <- aabbs.zipWithIndex) naive.add(b, idx)

    val wholeBounds = aabbs.tail.foldLeft(aabbs.head)((acc, b) => acc.combine(b))
    val octree = new OctreeStorage[AABox, Int](wholeBounds)
    for ((b, idx) <- aabbs.zipWithIndex) octree.add(b, idx)

    val Q_SIZE = 256
    val queries = for (i <- 0 until Q_SIZE) yield {
      val c = V3(i * 1.0, 0.0, 0.0)
      AABox(c - V3(2.0), c + V3(2.0))
    }

    report("octree_query (500 aabbs)", 50000L, i => octree.values(queries(i.toInt % Q_SIZE)).size.toDouble)
    report("naive_query (500 aabbs)", 50000L, i => naive.values(queries(i.toInt % Q_SIZE)).size.toDouble)
  }

  /** No Rust-vs-Scala accumulator parity expected here (unlike every other
    * case above): `HeightGridRandom`'s decorrelation hash is a faithful-
    * in-spirit (not bit-for-bit) port of Java's nested `Random` — see
    * `rust-utils/docs/porting.md`'s `utils.heightmap` section.
    */
  def benchHeightmap(): Unit = {
    val g = new HeightGridRandom(IntV2(256, 256), 1) with TiledHeightmap
    report(
      "heightmap_tiled_sample",
      200000L,
      i => {
        val x = ((i * 1000003L) % 255000L).toDouble / 1000.0
        val y = ((i * 999983L) % 255000L).toDouble / 1000.0
        g.heightAt(V2(x, y))
      },
    )
    report(
      "heightmap_bake_pow2",
      200L,
      i => {
        val g2 = HeightGridRandom(IntV2(256, 256), i.toInt)
        val baked: Pow2ArrayHeightGrid = g2.bakeToPow2Array
        baked.valueAt(IntV2(0, 0))
      },
    )
  }

  def main(args: Array[String]): Unit = {
    println(s"scalaUtils datastructures benchmark - $WARMUP_CHUNKS warmup chunks, then $CHUNKS measured chunks per op\n")
    benchBinHeapAddPoll()
    benchDisjointSetUnionFind()
    benchAvlTreeAddContains()
    benchGraphShortestPath()
    benchOctreeQueryVsNaive()
    benchHeightmap()
  }
}
