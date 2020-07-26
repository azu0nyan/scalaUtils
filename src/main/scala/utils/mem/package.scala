package utils
import utils.math._

package object mem {
  trait Data {
    def bits:Long
  }

  private class MultipliedData(val count:Long, val multiplier:Long) extends Data{
    override def bits: Long = count * multiplier
  }

  case class  Bit(override val count: Long) extends MultipliedData(count, 1)
  case class  Byte(override val count: Long) extends MultipliedData(count, 8)
  case class  Kibibyte(override val count: Long) extends MultipliedData(count, 8 * 1024)
  case class  Mebibyte(override val count: Long) extends MultipliedData(count, 8 * (1024 ^^ 2))
  case class  Gibibyte(override val count: Long) extends MultipliedData(count, 1)
  case class  Tebibyte(override val count: Long) extends MultipliedData(count, 1)
  case class  Pebibyte(override val count: Long) extends MultipliedData(count, 1)
}
