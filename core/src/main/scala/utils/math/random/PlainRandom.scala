package utils.math.random

import utils.abstractions.SeededIterable.SeededIterable
import utils.math.*
import utils.math.planar.V2

import scala.util.Random

object PlainRandom{
  type SEED = Long
  class PlainIntRandom(override val initialSeed: SEED) extends SeededIterable[Int, SEED] {
    override val generateNext: SEED => (Int, SEED) = seed => {
      val rand = new Random(seed)
      (rand.nextInt(), rand.nextInt())
    }
  }

  class PlainScalarRandom(override val initialSeed: SEED, from:Scalar = 0d , to:Scalar = 1d) extends SeededIterable[Scalar, SEED] {
    override val generateNext: SEED => (Scalar, SEED) = seed => {
      val rand = new Random(seed)
      (randInRange(from, to, rand), rand.nextInt())
    }
  }

  class PlainV2Random(override val initialSeed: SEED, from:V2 = V2.ZERO , to:V2 = V2(1d, 1d)) extends SeededIterable[V2, SEED] {
    override val generateNext: SEED => (V2, SEED) = seed => {
      val rand = new Random(seed)
      (randV2InRange(from, to, rand), rand.nextInt())
    }
  }

  class PlainV3Random(override val initialSeed: Long, from:V2 = V2.ZERO , to:V2 = V2(1d, 1d)) extends SeededIterable[V2, SEED] {
    override val generateNext: SEED => (V2, SEED) = seed => {
      val rand = new Random(seed)
      (randV2InRange(from, to, rand), rand.nextInt())
    }
  }

}

