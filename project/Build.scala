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

  lazy val scalanizer = Project(
    id   = "scalanizer",
    base = file(".")
  ) settings (
    sharedSettings ++ assemblySettings : _*
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "provided"),
    libraryDependencies ++= Seq("com.huawei.scalan" %% "scalan-common" % "0.2.9-SNAPSHOT"),
    libraryDependencies ++= Seq("com.huawei.scalan" %% "scalan-core" % "0.2.9-SNAPSHOT"),
    libraryDependencies ++= Seq("com.huawei.scalan" %% "scalan-meta" % "0.2.9-SNAPSHOT"),
    jarName in assembly := name.value + "_" + scalaVersion.value + "-" + version.value + "-fat.jar",
    assemblyOption in assembly ~= { _.copy(includeScala = false, includeDependency = true) }
  )
}