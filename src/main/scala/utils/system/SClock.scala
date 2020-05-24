package utils.system

import utils.math.Scalar


object SClock {

  def toSeconds(time: DiscreteTime): Scalar = time / 1000f

  type DiscreteTime = Long

  case class Timestamp(time: DiscreteTime, dt: DiscreteTime) {

    @inline def earlierThan(t: Timestamp): Boolean = time < t.time

    @inline  def laterThan(t: Timestamp): Boolean = time > t.time

    @inline  def simultaneouslyWith(t: Timestamp): Boolean = time == t.time

    @inline def simultaneouslyOrLater(t: Timestamp): Boolean = this.simultaneouslyWith(t) || this.laterThan(t)

    @inline def simultaneouslyOrEarlier(t: Timestamp): Boolean = this.simultaneouslyWith(t) || this.earlierThan(t)

    @inline def shorterThan(t: Timestamp): Boolean = dt < t.dt

    @inline def longerThan(t: Timestamp): Boolean = dt > t.dt


    def now(): Timestamp = {
      val x = SClock.now().time
      Timestamp(x, x - time)
    }

    @inline def since(ot: Timestamp): DiscreteTime = time - ot.time

    //def since(t:Timestamp):Timestamp = Timestamp(time - t.time)

    @inline def seconds: Scalar = toSeconds(time)

    @inline def dtSeconds: Scalar = toSeconds(dt)

  }


  val appStart: Timestamp = Timestamp(System.currentTimeMillis(), 0L)

  private var lastStamp:Timestamp = Timestamp(System.currentTimeMillis(), 0L)

  def now(): Timestamp = {
    val current = System.currentTimeMillis();
    if(current != lastStamp.time){
      lastStamp = Timestamp(current, 0L)
    }
    return lastStamp
  }

  def now(withDelta:DiscreteTime):Timestamp = Timestamp(System.currentTimeMillis(), withDelta)

}
