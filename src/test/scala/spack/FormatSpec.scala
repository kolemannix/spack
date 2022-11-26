package spack

import zio.test._
import scodec.bits._

object FormatSpec extends ZIOSpecDefault {
  def spec = suite("FormatSpec")(
    suite("Str")(
      test("FixStr") {
        val input  = Message.Str("foobear")
        val output = write(input)
        println(s"fixstr output: ${BitVector(output)}")
        assertTrue(parse(output) == Message.Str("foobear"))
      },
      test("str8") {
        val s      = List.fill(100)("x").mkString
        val input  = Message.Str(s)
        val output = write(input)
        println(s"str8 output: ${BitVector(output)}")
        assertTrue(parse(output) == Message.Str(s))
      },
      test("str16") {
        val s      = List.fill(1143)("x").mkString
        val input  = Message.Str(s)
        val output = write(input)
        println(s"str16 output: ${BitVector(output).toHex}")
        assertTrue(parse(output) == Message.Str(s))
      },
      test("str32") {
        val s      = List.fill(131071)("y").mkString
        val input  = Message.Str(s)
        val output = write(input)
        val parsed = parse(output)
        assertTrue(parsed == Message.Str(s))
      },
    ),
    suite("Utils")(
      test("intFromBytes") {
        val x     = 131071
        val byte0 = x.getByteBigEndian(0)
        val byte1 = x.getByteBigEndian(1)
        val byte2 = x.getByteBigEndian(2)
        val byte3 = x.getByteBigEndian(3)
        val y     = intFromBytesBigEndian(byte0, byte1, byte2, byte3)
        assertTrue(x == y)
      },
      test("getByteBigEndian positive") {
        val x = 1294967294
        assertTrue(
          x.getByteBigEndian(0) == bin"01001101".getByte(0),
          x.getByteBigEndian(1) == bin"00101111".getByte(0),
          x.getByteBigEndian(2) == bin"10100001".getByte(0),
          x.getByteBigEndian(3) == bin"11111110".getByte(0),
        )
      },
      test("Int.getByte negative") {
        val x = -1294967294
        // 1011_0010_1101_0000_0101_1110_0000_0010
        assertTrue(
          x.getByteBigEndian(0) == bin"10110010".getByte(0),
          x.getByteBigEndian(1) == bin"11010000".getByte(0),
          x.getByteBigEndian(2) == bin"01011110".getByte(0),
          x.getByteBigEndian(3) == bin"00000010".getByte(0),
        )
      },
    ),
  )
}
