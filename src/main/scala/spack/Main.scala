package spack

object Main extends App {
  val result = parse(Array[Byte](0xc1))
  println(result)
}

object Other extends App {
  println("We are there")
}
