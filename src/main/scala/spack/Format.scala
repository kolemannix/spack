package spack
import scodec.bits._

import scala.Conversion
import scala.collection.immutable
import scala.collection.mutable

import Constants._
import com.typesafe.scalalogging.Logger

private final val logger = Logger("spack")

extension (b: Byte) {
  inline def toUnsignedInt: Int = (b: Int) & 0xff
}

extension (i: Int)
  inline def getByteBigEndian(index: Int): Byte = inline index match {
    case 0 => (i >>> 24).toByte
    case 1 => (i >>> 16).toByte
    case 2 => (i >>> 8).toByte
    case 3 => i.toByte
  }

inline def int32FromBytes(byte0: Byte, byte1: Byte, byte2: Byte, byte3: Byte): Int = {
  (byte0 & 0xff) << 24 | (byte1 & 0xff) << 16 | (byte2 & 0xff) << 8.toByte | (byte3 & 0xff)
}
inline def int16FromBytes(byte2: Byte, byte3: Byte): Int = {
  (byte2 & 0xff) << 8.toByte | (byte3 & 0xff)
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

  def parseStr8(buffer: Array[Byte], offset: Int): ParseResult[Message.Str] = {
    val length  = buffer(offset + 1).toUnsignedInt
    val content = buffer.slice(offset + 2, offset + length + 2)
    if (content.length < length)
      ParseResult.fail(s"Underflow reading Str8: expected ${length} bytes, got ${content.length}", 0)
    else
      ParseResult.succeed(Message.Str(new String(content, "UTF-8")), offset + length + 2)
  }

  def parseStr16(buffer: Array[Byte], offset: Int): ParseResult[Message.Str] = {
    val lengthHi = buffer(offset + 1)
    val lengthLo = buffer(offset + 2)
    logger.debug(s"parseStr16 len hi: ${lengthHi}")
    logger.debug(s"parseStr16 len lo: ${lengthLo}")
    val length     = (lengthHi & 0xff) << 8 | lengthLo & 0xff
    val lengthEasy = int16FromBytes(lengthHi, lengthLo)
    logger.debug(s"parseStr16 len int: ${length}")
    logger.debug(s"parseStr16 len int (easy): ${lengthEasy}")
    val content = buffer.slice(offset + 3, offset + length + 3)
    if (content.length < length)
      ParseResult.fail(s"Underflow reading str16: expected ${length} bytes, got ${content.length}", 0)
    else
      ParseResult.succeed(Message.Str(new String(content, "UTF-8")), offset + length + 3)
  }

  def parseStr32(buffer: Array[Byte], offset: Int): ParseResult[Message.Str] = {
    val length  = int32FromBytes(buffer(offset + 1), buffer(offset + 2), buffer(offset + 3), buffer(offset + 4))
    val content = buffer.slice(offset + 5, offset + length + 5)

    logger.debug(s"Str32 length is: ${length}")

    if (content.length < length)
      ParseResult.fail(s"Underflow reading str32: expected ${length} bytes, got ${content.length}", offset + content.length + 5)
    else
      ParseResult.succeed(Message.Str(new String(content, "UTF-8")), offset + length + 5)
  }

  /** fixstr stores a byte array whose length is upto 31 bytes:
    * \+--------+========+
    * \|101XXXXX| data |
    * \+--------+========+
    * XXXXX is a 5-bit unsigned integer which represents N
    */
  def parseFixStr(length: Int, buffer: Array[Byte], offset: Int): ParseResult[Message.Str] = {
    val content = buffer.slice(offset + 1, offset + length + 1)
    if (content.length < length)
      ParseResult.fail(s"Underflow reading FixStr: expected ${length} bytes, got ${content.length}", offset + content.length)
    else
      ParseResult.succeed(Message.Str(new String(content, "UTF-8")), offset + length + 1)
  }

  /** Checks for the FixStr format, and if it matches,
    * returns the length from the last 5 bits as an integer
    */
  object FixStrFormat:
    def unapply(b: Byte): Option[Int] =
      if (b >= FIX_STR_MIN && b <= FIX_STR_MAX) {
        val length = (b & 0x1f.toByte)
        logger.debug(s"FixStrLen: ${length}")
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
      logger.debug("Writing FixStr")
      // Write out a FixStr
      val output = new Array[Byte](1 + len)
      // 101 | xxxxx
      val formatBit: Byte = (FIX_STR_SENTINEL | len.toByte).toByte
      logger.debug(s"formatBit=${formatBit.toUnsignedInt}")
      output.update(0, formatBit)
      // @Profile: with while loop, zipWithIndex, etc
      writeStringFrom(output, 1, s)
      output
    } else if (len <= 255) {
      logger.debug("Writing str8")
      // Write out a Str8
      val output = new Array[Byte](2 + len)
      output.update(0, STR_8_SENTINEL)
      val lenUByte: Byte = len.toByte
      output.update(1, lenUByte)
      writeStringFrom(output, 2, s)
      output
    } else if (len <= 65535) {
      logger.debug("Writing str16")
      // Write out a Str16
      val output = new Array[Byte](3 + len)
      output.update(0, STR_16_SENTINEL)
      val lenUpperBigEndian = len.getByteBigEndian(2)
      val lenLowerBigEndian = len.getByteBigEndian(3)
      logger.debug(s"Writing str16 len hi: ${lenUpperBigEndian}")
      logger.debug(s"Writing str16 len lo: ${lenLowerBigEndian}")
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

object Map {
  object FixMapFormat {
    def unapply(b: Byte): Option[Int] =
      if ((b & 0xf0) == 0x80) {
        val length = b & 0x0f
        logger.debug(s"FixMapLen: ${length}")
        Some(length)
      } else {
        None
      }
  }

  def writeMap(map: Message.Map): Array[Byte] = {
    def writeMapEntries(output: mutable.Buffer[Byte], entries: Seq[(Message, Message)]) = {
      map.entries.foreach { case (key, value) =>
        val keyArr = write(key)
        output ++= keyArr
        val valueArr = write(value)
        output ++= valueArr
      }
    }
    map.entries.size match {
      case len if len < 16 => {
        val output = mutable.Buffer[Byte]()
        // Format Byte
        output += (0x80 | len).toByte
        writeMapEntries(output, map.entries)
        output.toArray
      }
      case len if len < 65535 => {
        // Map16
        val output = mutable.Buffer[Byte]()
        // Format Byte
        output += 0xde.toByte
        // Length bytes
        val lenUpperBigEndian = len.getByteBigEndian(2)
        val lenLowerBigEndian = len.getByteBigEndian(3)
        output += lenUpperBigEndian
        output += lenLowerBigEndian
        writeMapEntries(output, map.entries)
        output.toArray
      }
      case len => {
        // Map32
        val output = mutable.Buffer[Byte]()
        // Format Byte
        output += 0xde.toByte
        // Length bytes
        output += len.getByteBigEndian(0)
        output += len.getByteBigEndian(1)
        output += len.getByteBigEndian(2)
        output += len.getByteBigEndian(3)
        writeMapEntries(output, map.entries)
        output.toArray
      }
    }
  }

  def parseFixMap(length: Int, buffer: Array[Byte], offset: Int): ParseResult[Message.Map] = {
    logger.debug("parseFixMap")
    val objectStartOffset = offset + 1
    parseMapObjects(buffer, length, objectStartOffset)
  }

  def parseMapObjects(buffer: Array[Byte], length: Int, startOffset: Int): ParseResult[Message.Map] = {
    val pairs  = new Array[(Message, Message)](length)
    var index  = 0
    var offset = startOffset
    while (index < length) {
      innerParse(buffer, offset) match {
        case Left(err) => 
          return ParseResult.fail(s"Failed to parse map key: ${err.message}", offset)
        case Right(keySuccess) =>
          innerParse(buffer, keySuccess.offset) match {
            case Left(err) => 
              return ParseResult.fail(s"Failed to parse map value: ${err.message}", keySuccess.offset)
            case Right(valueSuccess) =>
              pairs.update(index, (keySuccess.message, valueSuccess.message))
              index += 1
              offset = valueSuccess.offset
          }
      }
    }
    ParseResult.succeed(Message.Map(pairs.toSeq), offset)
  }

}

enum Message {
  case Bool(value: Boolean)
  case Bin(bytes: immutable.ArraySeq[Byte])
  case Str(string: String)
  case Map(entries: Seq[(Message, Message)])
  case Nil
}

case class ParseError(message: String, offset: Int)

case class ParseSuccess[+A <: Message](message: A, offset: Int)

type ParseResult[+A <: Message] = Either[ParseError, ParseSuccess[A]]
object ParseResult {
  def succeed[A <: Message](message: A, offset: Int): ParseResult[A]   = Right(ParseSuccess(message, offset))
  def fail[A <: Message](message: String, offset: Int = 0): ParseResult[A] = Left(ParseError(message, offset))
}
// extension[A <: Message] (res: ParseResult[A]) {
//   def flatMap(other: ParseResult[A]): ParseResult[A] = {
//     res.flatMap(other)
//   }
// }

object ParseError:
  def unimplemented = ParseError("UNIMPLEMENTED", 0)

def parseBin8(buffer: Array[Byte], offset: Int): ParseResult[Message.Bin] = {
  val length  = buffer(offset + 1).toUnsignedInt
  val content = buffer.slice(offset + 2, offset + length + 2)
  if (content.length < length)
    ParseResult.fail(s"Underflow reading Bin8: expected ${length} bytes, got ${content.length}", offset)
  else {
    val msg: Message.Bin = Message.Bin(immutable.ArraySeq.unsafeWrapArray(content))
    ParseResult.succeed(msg, offset + length + 2)
  }
}

def write(message: Message): Array[Byte] =
  message match {
    // Base Cases
    case Message.Nil         => Array(Constants.NIL)
    case Message.Bool(true)  => Array(Constants.TRUE)
    case Message.Bool(false) => Array(Constants.FALSE)
    case Message.Str(s)      => Str.writeString(s)
    case Message.Bin(s)      => throw new RuntimeException("WRITE bin unimplemented")
    // Recursive cases
    case map @ Message.Map(_) => Map.writeMap(map)
  }

/** Returns the number of bytes consumed during parsing
  */
def innerParse(buffer: Array[Byte], offset: Int): Either[ParseError, ParseSuccess[Message]] = {
  buffer(offset) match {
    case `NIL`                    => ParseResult.succeed(Message.Nil, offset + 1)
    case `FALSE`                  => ParseResult.succeed(Message.Bool(false), offset + 1)
    case `TRUE`                   => ParseResult.succeed(Message.Bool(true), offset + 1)
    case `BIN_8`                  => parseBin8(buffer, offset)
    case Str.`STR_8_SENTINEL`     => Str.parseStr8(buffer, offset)
    case Str.`STR_16_SENTINEL`    => Str.parseStr16(buffer, offset)
    case Str.`STR_32_SENTINEL`    => Str.parseStr32(buffer, offset)
    case Str.FixStrFormat(length) => Str.parseFixStr(length, buffer, offset)
    case Map.FixMapFormat(length) => Map.parseFixMap(length, buffer, offset)
    case other                    => ParseResult.fail("UNIMPLEMENTED", offset)
  }
}

def parse(buffer: Array[Byte]): ParseResult[Message] = {
  innerParse(buffer, offset = 0)
}
