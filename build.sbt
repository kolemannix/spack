ThisBuild / scalaVersion := "3.2.0"
val scala2Version = "2.13.8"

lazy val root = project.in(file(".")).settings(
  testFrameworks ++= Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  Compile / scalacOptions ++= Seq(
    "-Yexplicit-nulls"
  ),
  libraryDependencies ++= Seq(
    "org.scodec" %% "scodec-bits" % "1.1.34",
    // "dev.zio"       %% "zio"         % "2.0.3",
    "dev.zio" %% "zio-test"     % "2.0.3" % Test,
    "dev.zio" %% "zio-test-sbt" % "2.0.3" % Test,
  ),
)
