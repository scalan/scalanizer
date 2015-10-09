import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object ScalanBuild extends Build {
  lazy val sharedSettings = Defaults.coreDefaultSettings ++ Seq(
//    scalaVersion := "2.11.7",
    scalaOrganization := "org.scala-lang.virtualized",
    scalaVersion := "2.11.2",
    version := "0.0.4-SNAPSHOT",
    organization := "com.huawei.scalan"
  )

  def scalanDependency(name: String) = "com.huawei.scalan" %% s"scalan-$name" % "0.2.11-SNAPSHOT"

  lazy val scalanizer = Project(
    id   = "scalanizer",
    base = file(".")
  ) settings (
    sharedSettings ++ assemblySettings : _*
  ) settings (
    libraryDependencies ++= Seq(
      "org.scala-lang.virtualized" % "scala-reflect" % scalaVersion.value % "provided",
      "org.scala-lang.virtualized" % "scala-compiler" % scalaVersion.value % "provided",
      scalanDependency("core"),
      scalanDependency("meta").exclude("org.scala-lang", "scala-compiler")),
    publishArtifact in (Compile, packageBin) := false,
    assemblyOption in assembly ~= { _.copy(includeScala = false, includeDependency = true) },
    artifact in (Compile, assembly) := {
      val art = (artifact in (Compile, assembly)).value
      art.copy(classifier = Some("assembly"))
    },
    addArtifact(artifact in (Compile, assembly), assembly)
  )
}
