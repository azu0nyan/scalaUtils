package utils

import utils.math._

package object mem {
  sealed trait Data {
    def bits: Long
    def bytes: Long = bits >> 3
    def kilobytes: Long = bits >> (3 + 10L)
    def megabytes: Long = bits >> (3 + 20L)
    def gigabytes: Long = bits >> (3 + 30L)
    def terabytes: Long = bits >> (3 + 40L)
    def petabytes: Long = bits >> (3 + 50L)
  }
  import nest._


  implicit class toData(x: Long) {
    def Bit: DataBit = DataBit(x)
    def Byte: DataByte = DataByte(x)
    def KiB: Kibibyte = Kibibyte(x)
    def MiB: Mebibyte = Mebibyte(x)
    def GiB: Gibibyte = Gibibyte(x)
    def TiB: Tebibyte = Tebibyte(x)
    def PiB: Pebibyte = Pebibyte(x)
  }

  type Kilobyte = Kibibyte
  type Megabyte = Mebibyte
  type Gigabyte = Kibibyte
  type Terabyte = Tebibyte
  type Petabyte = Pebibyte
  object nest {



    case class DataBit(count: Long) extends Data {
      val multiplier: Long = 1
      override val bits: Long = count * multiplier
    }
    case class DataByte(count: Long) extends Data {
      val multiplier: Long = 8
      override val bits: Long = count * multiplier
    }
    case class Kibibyte(count: Long) extends Data {
      val multiplier: Long = 8 * 1024
      override val bits: Long = count * multiplier
    }
    case class Mebibyte(count: Long) extends Data {
      val multiplier: Long = 8 * (1024 ^^ 2)
      override val bits: Long = count * multiplier
    }
    case class Gibibyte(count: Long) extends Data {
      val multiplier: Long = 8 * (1024 ^^ 3)
      override val bits: Long = count * multiplier
    }
    case class Tebibyte(count: Long) extends Data {
      val multiplier: Long = 8 * (1024 ^^ 4)
      override val bits: Long = count * multiplier
    }
    case class Pebibyte(count: Long) extends Data {
      val multiplier: Long = 8 * (1024 ^^ 5)
      override val bits: Long = count * multiplier
    }
  }
}
