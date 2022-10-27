ThisBuild / scalaVersion := "3.2.0"
Compile / scalacOptions ++= Seq(
  "-Yexplicit-nulls"
)
lazy val root = project.in(file("."))
