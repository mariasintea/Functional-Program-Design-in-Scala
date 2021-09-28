course := "progfun2"
assignment := "codecs"

scalaVersion := "2.13.5"
scalacOptions ++= Seq("-deprecation")
libraryDependencies ++= Seq(
  "org.typelevel" %% "jawn-parser" % "0.14.2",
  "org.scalacheck" %% "scalacheck" % "1.14.2" % Test,
  "org.scalameta" %% "munit" % "0.7.22" % Test
)

testFrameworks += new TestFramework("munit.Framework")
