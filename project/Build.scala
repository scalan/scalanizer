import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object ScalanizerBuild extends Build {
  lazy val sharedSettings = Defaults.coreDefaultSettings ++ Seq(
    scalaVersion := "2.11.2",
	crossScalaVersions := Seq("2.11.2", "2.11.6"),
    crossVersion := CrossVersion.full,
    version := "0.0.2-SNAPSHOT",
    organization := "com.huawei.scalan",
	ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }
  )

  lazy val scalanizer = Project(
    id   = "scalanizer",
    base = file(".")
  ) settings (
    sharedSettings ++ assemblySettings : _*
  ) settings (
    libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % scalaVersion.value % "provided",
    libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % scalaVersion.value % "provided",
    libraryDependencies ++= Seq("com.huawei.scalan" %% "scalan-common" % "0.2.9-SNAPSHOT"),
    libraryDependencies ++= Seq("com.huawei.scalan" %% "scalan-core" % "0.2.9-SNAPSHOT"),
    libraryDependencies ++= Seq("com.huawei.scalan" %% "scalan-meta" % "0.2.9-SNAPSHOT"),
    jarName in assembly := name.value + "_" + scalaVersion.value + "-" + version.value + "-fat.jar",
    assemblyOption in assembly ~= { _.copy(includeScala = false, includeDependency = true) }
  )
}