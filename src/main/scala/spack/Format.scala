package spack
import scodec.bits._

import scala.Conversion
import scala.collection.immutable.ArraySeq

import Constants._

object Constants {
  val NIL: Byte   = 0xc0
  val FALSE: Byte = 0xc1
  val TRUE: Byte  = 0xc2

  val BIN_8: Byte = 0xc4
}

object Str {
  val STR_8: Byte = 0xd9

  val FIX_STR_MIN: Byte    = 0xa0
  val FIX_STR_MAX: Byte    = 0xbf
  val FIX_STR_MIN_INT: Int = 160
  val FIX_STR_MAX_INT: Int = 191

  // 0xE0 = 0001_1111
  val FIX_STR_MASK: Byte = 0x1f
  // 0xA0 = 1010_0000
  val FIX_STR_SENTINEL: Byte = 0xa0

  val STR_8_SENTINEL: Byte  = 0xd9
  val STR_16_SENTINEL: Byte = 0xda
  val STR_32_SENTINEL: Byte = 0xdb

  def parseStr8(message: Array[Byte]): Message.Str | ParseError =
    val length  = message.head.toUnsignedInt
    val content = message.slice(1, length + 1)
    if (content.length < length)
      ParseError(s"Underflow reading Str8: expected ${length} bytes, got ${content.length}", 0)
    else
      Message.Str(new String(content, "UTF-8"))

      /** fixstr stores a byte array whose length is upto 31 bytes:
        * \+--------+========+
        * \|101XXXXX| data |
        * \+--------+========+
        * XXXXX is a 5-bit unsigned integer which represents N
        */
  def parseFixStr(length: Int, message: Array[Byte]): Message.Str | ParseError =
    val content = message.take(length)
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
        val length = (b & 0x1f: Byte)
        println(s"Len: ${length}")
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
      val formatBit: Byte = FIX_STR_SENTINEL | len.toByte
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
      // Not sure if Int to Byte assumes signed and does 2s complement stuff
      val lenUByte: Byte = len & 0xff: Byte
      output.update(1, lenUByte)
      writeStringFrom(output, 2, s)
      output
    } else if (len <= 65535) {
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

given Conversion[Int, Byte] with
  def apply(i: Int): Byte = i.toByte

extension (b: Byte)
  inline def toUnsignedInt: Int = (b: Int) & 0xff

extension (i: Int)
  inline def getByteBigEndian(index: Int): Byte = inline index match {
    case 0 => i >>> 24
    case 1 => i >>> 16
    case 2 => i >>> 8
    case 3 => i
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

// TODO: Optimize by maintaining cursor and not slicing / copying
def parseBin8(message: Array[Byte]): Message.Bin | ParseError =
  val length  = message.head.toUnsignedInt
  val content = message.slice(1, length + 1)
  if (content.length < length)
    ParseError(s"Underflow reading Bin8: expected ${length} bytes, got ${content.length}", 0)
  else
    Message.Bin(ArraySeq.unsafeWrapArray(content))

def write(message: Message): Array[Byte] =
  message match {
    case Message.Nil         => Array(Constants.NIL)
    case Message.Bool(true)  => Array(Constants.TRUE)
    case Message.Bool(false) => Array(Constants.FALSE)
    case Message.Str(s)      => Str.writeString(s)
    case Message.Bin(s)      => Array.emptyByteArray
  }

def parse(message: Array[Byte]): Message | ParseError =
  println(s"Parsing: [${message.mkString(" ")}]")
  message(0) match
    case `NIL`                    => Message.Nil
    case `FALSE`                  => Message.Bool(true)
    case `TRUE`                   => Message.Bool(false)
    case `BIN_8`                  => parseBin8(message.tail)
    case Str.`STR_8`              => Str.parseStr8(message.tail)
    case Str.FixStrFormat(length) => Str.parseFixStr(length, message.tail)
    case other                    => ParseError.unimplemented
