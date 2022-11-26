package spack
import scodec.bits._

import scala.Conversion
import scala.collection.immutable.ArraySeq

import Constants._

extension (b: Byte)
  inline def toUnsignedInt: Int = (b: Int) & 0xff

extension (i: Int)
  inline def getByteBigEndian(index: Int): Byte = inline index match {
    case 0 => (i >>> 24).toByte
    case 1 => (i >>> 16).toByte
    case 2 => (i >>> 8).toByte
    case 3 => i.toByte
  }

def intFromBytesBigEndian(byte0: Byte, byte1: Byte, byte2: Byte, byte3: Byte): Int = {
  (byte0 & 0xff) << 24 | (byte1 & 0xff) << 16 | (byte2 & 0xff) << 8.toByte | (byte3 & 0xff)
}

object Constants {
  val NIL: Byte   = 0xc0.toByte
  val FALSE: Byte = 0xc1.toByte
  val TRUE: Byte  = 0xc2.toByte

  val BIN_8: Byte = 0xc4.toByte
}

object Str {
  val FIX_STR_MIN: Byte    = 0xa0.toByte
  val FIX_STR_MAX: Byte    = 0xbf.toByte
  val FIX_STR_MIN_INT: Int = 160
  val FIX_STR_MAX_INT: Int = 191

  // 0xE0 = 0001_1111
  val FIX_STR_MASK: Byte = 0x1f
  // 0xA0 = 1010_0000
  val FIX_STR_SENTINEL: Byte = 0xa0.toByte

  val STR_8_SENTINEL: Byte  = 0xd9.toByte
  val STR_16_SENTINEL: Byte = 0xda.toByte
  val STR_32_SENTINEL: Byte = 0xdb.toByte

  def parseStr8(message: Array[Byte]): Message.Str | ParseError = {
    val length  = message(1).toUnsignedInt
    val content = message.slice(2, length + 2)
    if (content.length < length)
      ParseError(s"Underflow reading Str8: expected ${length} bytes, got ${content.length}", 0)
    else
      Message.Str(new String(content, "UTF-8"))
  }

  def parseStr16(message: Array[Byte]): Message.Str | ParseError = {
    val lengthHi = message(1)
    val lengthLo = message(2)
    val length   = lengthHi << 8 | lengthLo
    val content  = message.slice(3, length + 3)
    if (content.length < length)
      ParseError(s"Underflow reading str16: expected ${length} bytes, got ${content.length}", 0)
    else
      Message.Str(new String(content, "UTF-8"))
  }

  def parseStr32(message: Array[Byte]): Message.Str | ParseError = {
    val lengthBits = message.slice(1, 5)
    val length     = intFromBytesBigEndian(message(1), message(2), message(3), message(4))
    val content    = message.slice(5, length + 5)

    println(s"lengthBits: ${lengthBits.mkString(", ")}")
    println(s"Str32 length is: ${length}")

    if (content.length < length)
      ParseError(s"Underflow reading str32: expected ${length} bytes, got ${content.length}", 0)
    else
      Message.Str(new String(content, "UTF-8"))
  }

  /** fixstr stores a byte array whose length is upto 31 bytes:
    * \+--------+========+
    * \|101XXXXX| data |
    * \+--------+========+
    * XXXXX is a 5-bit unsigned integer which represents N
    */
  def parseFixStr(length: Int, message: Array[Byte]): Message.Str | ParseError =
    val content = message.slice(1, length + 1)
    if (content.length < length)
      ParseError(s"Underflow reading FixStr: expected ${length} bytes, got ${content.length}", 0)
    else
      Message.Str(new String(content, "UTF-8"))

    /** Checks for the FixStr format, and if it matches,
      * returns the length from the last 5 bits as an integer
      */
  object FixStrFormat:
    def unapply(b: Byte): Option[Int] =
      if (b >= FIX_STR_MIN && b <= FIX_STR_MAX) {
        val length = (b & 0x1f.toByte)
        println(s"FixStrLen: ${length}")
        Some(length)
      } else {
        None
      }

  // @Profile: whether inline matters after JIT
  def writeStringFrom(dest: Array[Byte], from: Int, string: String): Unit = {
    var i = 0
    string.foreach { c =>
      dest.update(from + i, c.toByte)
      i += 1
    }
  }

  def writeString(s: String): Array[Byte] =
    val len = s.length
    // 31 == 0b0001_1111
    if (len <= 31) {
      println("Writing FixStr")
      // Write out a FixStr
      val output = new Array[Byte](1 + len)
      // 101 | xxxxx
      val formatBit: Byte = (FIX_STR_SENTINEL | len.toByte).toByte
      println(s"formatBit=${formatBit.toUnsignedInt}")
      output.update(0, formatBit)
      // @Profile: with while loop, zipWithIndex, etc
      writeStringFrom(output, 1, s)
      output
    } else if (len <= 255) {
      println("Writing str8")
      // Write out a Str8
      val output = new Array[Byte](2 + len)
      output.update(0, STR_8_SENTINEL)
      val lenUByte: Byte = len.toByte
      output.update(1, lenUByte)
      writeStringFrom(output, 2, s)
      output
    } else if (len <= 65535) {
      println("Writing str16")
      // Write out a Str16
      val output = new Array[Byte](3 + len)
      output.update(0, STR_16_SENTINEL)
      val lenUpperBigEndian = len.getByteBigEndian(2)
      val lenLowerBigEndian = len.getByteBigEndian(3)
      output.update(1, lenUpperBigEndian)
      output.update(2, lenLowerBigEndian)
      writeStringFrom(output, 3, s)
      output
    } else {
      // Since len is an int, and a java string cannot be longer than 2^32,
      // Write out a Str32
      val output = new Array[Byte](5 + len)
      output.update(0, STR_32_SENTINEL)
      output.update(1, len.getByteBigEndian(0))
      output.update(2, len.getByteBigEndian(1))
      output.update(3, len.getByteBigEndian(2))
      output.update(4, len.getByteBigEndian(3))
      writeStringFrom(output, 5, s)
      output
    }

}

enum Message {
  case Bool(value: Boolean)
  case Bin(bytes: ArraySeq[Byte])
  case Str(string: String)
  case Nil
}

case class ParseError(message: String, offset: Int)

object ParseError:
  def unimplemented = ParseError("UNIMPLEMENTED", 0)

def parseBin8(message: Array[Byte]): Message.Bin | ParseError = {
  val length  = message(1).toUnsignedInt
  val content = message.slice(2, length + 2)
  if (content.length < length)
    ParseError(s"Underflow reading Bin8: expected ${length} bytes, got ${content.length}", 0)
  else
    Message.Bin(ArraySeq.unsafeWrapArray(content))
}

def write(message: Message): Array[Byte] =
  message match {
    case Message.Nil         => Array(Constants.NIL)
    case Message.Bool(true)  => Array(Constants.TRUE)
    case Message.Bool(false) => Array(Constants.FALSE)
    case Message.Str(s)      => Str.writeString(s)
    case Message.Bin(s)      => Array.emptyByteArray
  }

def parse(message: Array[Byte]): Message | ParseError = {
  message(0) match {
    case `NIL`                    => Message.Nil
    case `FALSE`                  => Message.Bool(true)
    case `TRUE`                   => Message.Bool(false)
    case `BIN_8`                  => parseBin8(message)
    case Str.`STR_8_SENTINEL`     => Str.parseStr8(message)
    case Str.`STR_16_SENTINEL`    => Str.parseStr16(message)
    case Str.`STR_32_SENTINEL`    => Str.parseStr32(message)
    case Str.FixStrFormat(length) => Str.parseFixStr(length, message)
    case other                    => ParseError.unimplemented
  }
}
