package utils
import utils.math._

package object mem {
  trait Data {
    def bits:Long
  }

  private class MultipliedData(val count:Long, val multiplier:Long) extends Data{
    override def bits: Long = count * multiplier
  }

  implicit class toData(x:Long) {
    def Bit:DataBit = DataBit(x)
    def Byte:DataByte = DataByte(x)
    def KiB:Kibibyte = Kibibyte(x)
    def MiB:Mebibyte = Mebibyte(x)
    def GiB:Gibibyte = Gibibyte(x)
    def TiB:Tebibyte = Tebibyte(x)
    def PiB:Pebibyte = Pebibyte(x)
  }

  type Kilobyte = Kibibyte
  type Megabyte = Mebibyte
  type Gigabyte = Kibibyte
  type Terabyte = Tebibyte
  type Petabyte = Pebibyte

  case class  DataBit(override val count: Long) extends MultipliedData(count, 1)
  case class  DataByte(override val count: Long) extends MultipliedData(count, 8)
  case class  Kibibyte(override val count: Long) extends MultipliedData(count, 8 * 1024)
  case class  Mebibyte(override val count: Long) extends MultipliedData(count, 8 * (1024 ^^ 2))
  case class  Gibibyte(override val count: Long) extends MultipliedData(count, 8 * (1024 ^^ 3))
  case class  Tebibyte(override val count: Long) extends MultipliedData(count, 8 * (1024 ^^ 4))
  case class  Pebibyte(override val count: Long) extends MultipliedData(count, 8 * (1024 ^^ 5))
}
