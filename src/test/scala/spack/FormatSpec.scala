package spack

import zio.test._
import scodec.bits._

object FormatSpec extends ZIOSpecDefault {
  def spec = suite("FormatSpec")(
    suite("Str")(
      test("FixStr") {
        val input  = Message.Str("foobear")
        val output = write(input)
        assertTrue(parse(output) == Right(ParseSuccess(Message.Str("foobear"), 8)))
      },
      test("str8") {
        val s      = List.fill(100)("x").mkString
        val input  = Message.Str(s)
        val output = write(input)
        assertTrue(parse(output).toOption.get.message == Message.Str(s))
      },
      test("str16") {
        val s      = List.fill(1143)("x").mkString
        val input  = Message.Str(s)
        val output = write(input)
        assertTrue(parse(output).toOption.get.message == Message.Str(s))
      },
      test("str16 higher") {
        val s      = List.fill(32731)("i").mkString
        val input  = Message.Str(s)
        val output = write(input)
        assertTrue(parse(output).toOption.get.message == Message.Str(s))
      },
      test("str32") {
        val s      = List.fill(131071)("y").mkString
        val input  = Message.Str(s)
        val output = write(input)
        val parsed = parse(output)
        assertTrue(parsed.toOption.get.message == Message.Str(s))
      },
    ),
    suite("Map")(
      test("FixMap") {
        val input = Message.Map(Seq(
          Message.Str("foo")  -> Message.Str("bear"),
          Message.Str("bar")  -> Message.Str("BAR"),
          Message.Bool(false) -> Message.Bool(true),
        ))
        val output = write(input)
        val parsed = parse(output)
        BitVector(output).printHexDump()
        assertTrue(parsed.toOption.get.message == input)
      },
      test("Map16") {
        val input = Message.Map(
          (1 to 100000).map(x => (Message.Str(x.toString) -> Message.Bool(x % 2 == 0)))
        )
        val start = System.currentTimeMillis()
        val output = write(input)
        val parsed = parse(output)
        val end = System.currentTimeMillis()
        // BitVector(output).printHexDump()
        println(s"Elapsed: ${end - start}ms")
        assertTrue(parsed.toOption.get.message == input)
      },
    ),
    suite("Utils")(
      test("intFromBytes") {
        val x     = 131071
        val byte0 = x.getByteBigEndian(0)
        val byte1 = x.getByteBigEndian(1)
        val byte2 = x.getByteBigEndian(2)
        val byte3 = x.getByteBigEndian(3)
        val y     = int32FromBytes(byte0, byte1, byte2, byte3)
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
