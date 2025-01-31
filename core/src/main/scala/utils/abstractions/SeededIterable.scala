package utils.abstractions

object SeededIterable {



  type NextAndSeed[T, Seed] = (T, Seed)


  trait SeededIterable[T, Seed] extends Iterable[T]{
    /*should be pure  Seed => NextAndSeed[T, Seed ]*/
    val generateNext:Seed => NextAndSeed[T, Seed ]

    def initialSeed:Seed

    override def iterator: Iterator[T] = new SeededIterator[T, Seed] {
      override val generateNext: Seed => (T, Seed) = SeededIterable.this.generateNext

      override val initialSeed: Seed = SeededIterable.this.initialSeed
    }
  }

  trait SeededIterator[T, Seed ] extends Iterator[T] {

    val generateNext:Seed => NextAndSeed[T, Seed]

    val initialSeed:Seed

    private var seed: Seed = initialSeed

    //iterator interface

    override def next(): T = {
      val ns = generateNext(seed)
      seed = ns._2
      return ns._1
    }

    override def hasNext: Boolean = true

    override def clone(): SeededIterator[T, Seed] = new SeededIterator[T, Seed] {
      override val generateNext: Seed => (T, Seed) = SeededIterator.this.generateNext

      override val initialSeed: Seed = SeededIterator.this.seed
    }

    def seqFromHere:SeededIterable[T, Seed] = new SeededIterable[T, Seed] {
      override val generateNext: Seed => (T, Seed) = SeededIterator.this.generateNext

      override def initialSeed: Seed = SeededIterator.this.seed
    }

  }



}
