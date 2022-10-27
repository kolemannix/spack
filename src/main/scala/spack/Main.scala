package spack

import scala.Conversion
import Constants._
import scala.collection.immutable.ArraySeq
import java.lang.Byte.toUnsignedInt

object Constants {
  val NIL: Byte   = 0xc0
  val FALSE: Byte = 0xc1
  val TRUE: Byte  = 0xc2

  val BIN_8: Byte = 0xc4

  val STR_8: Byte = 0xd9

  val FIX_STR_MIN: Byte = 0xa0
  val FIX_STR_MAX: Byte = 0xbf

  // 0xE0 = 1110_0000
  val FIX_STR_MASK: Byte = 0xe0
}

given Conversion[Int, Byte] with
  def apply(i: Int): Byte = i.toByte

extension (b: Byte)
  def toUnsignedInt: Int = java.lang.Byte.toUnsignedInt(b)

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

def parseStr8(message: Array[Byte]): Message.Str | ParseError =
  val length  = message.head.toUnsignedInt
  val content = message.slice(1, length + 1)
  if (content.length < length)
    ParseError(s"Underflow reading Bin8: expected ${length} bytes, got ${content.length}", 0)
  else
    Message.Str(new String(content, "UTF-8"))

    /** fixstr stores a byte array whose length is upto 31 bytes:
      * \+--------+========+
      * \|101XXXXX| data |
      * \+--------+========+
      * XXXXX is a 5-bit unsigned integer which represents N
      */
def parseFixStr(message: Array[Byte]): Message.Str | ParseError =
  val length  = java.lang.Byte.toUnsignedInt(message.head)
  val content = message.slice(1, length + 1)
  if (content.length < length)
    ParseError(s"Underflow reading Bin8: expected ${length} bytes, got ${content.length}", 0)
  else
    Message.Str(new String(content, "UTF-8"))

/** Checks for the FixStr format, and if it matches,
  * returns the length from the last 5 bits as an integer
  */
def isFixStrFormat(b: Byte): Option[Int] =
  val unsigned = b.toUnsignedInt
  if (unsigned >= Constants.FIX_STR_MIN && unsigned <= Constants.FIX_STR_MAX)
  val length = b && 0xe0: Byte

def parse(message: Array[Byte]): Message | ParseError =
  println(s"Parsing: [${message.mkString(" ")}]")
  message(0) match {
    case `NIL`                  => Message.Nil
    case `FALSE`                => Message.Bool(true)
    case `TRUE`                 => Message.Bool(false)
    case other =>
      
    case b if isFixStrFormat(b) => parseBin8(message.tail)
  }

object Main extends App {
  val result = parse(Array[Byte](0xc1))
  result.toString
  println(result)
}

object Other extends App {
  println("We are there")
}
