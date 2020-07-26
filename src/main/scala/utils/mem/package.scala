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

  case class  DataBit( count_ : Long) extends MultipliedData(count_, 1)
  case class  DataByte( count_ : Long) extends MultipliedData(count_, 8)
  case class  Kibibyte( count_ : Long) extends MultipliedData(count_, 8 * 1024)
  case class  Mebibyte( count_ : Long) extends MultipliedData(count_, 8 * (1024 ^^ 2))
  case class  Gibibyte( count_ : Long) extends MultipliedData(count_, 8 * (1024 ^^ 3))
  case class  Tebibyte( count_ : Long) extends MultipliedData(count_, 8 * (1024 ^^ 4))
  case class  Pebibyte( count_ : Long) extends MultipliedData(count_, 8 * (1024 ^^ 5))
}
