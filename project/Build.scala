import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object ScalanBuild extends Build {
  lazy val sharedSettings = Defaults.coreDefaultSettings ++ Seq(
    scalaVersion := "2.11.6",
    crossVersion := CrossVersion.full,
    version := "0.0.1",
    organization := "com.huawei"
  )

  lazy val ScalanPlugin = Project(
    id   = "scalan-plugin",
    base = file(".")
  ) settings (
    sharedSettings ++ assemblySettings : _*
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "provided"),
    jarName in assembly := name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
    assemblyOption in assembly ~= { _.copy(includeScala = false) }
  )
}