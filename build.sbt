name := "scala-with-cats"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.scalatest" %% "scalatest" % "3.1.1"
)

scalacOptions ++= Seq("-Xfatal-warnings", "-Ypartial-unification")
