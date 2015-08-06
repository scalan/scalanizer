import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object ScalanBuild extends Build {
  lazy val sharedSettings = Defaults.coreDefaultSettings ++ Seq(
//    scalaVersion := "2.11.6",
    scalaOrganization := "org.scala-lang.virtualized",
    scalaVersion := "2.11.2",
    version := "0.0.2",
    organization := "com.huawei"
  )

  lazy val scalanizer = Project(
    id   = "scalanizer",
    base = file(".")
  ) settings (
    sharedSettings ++ assemblySettings : _*
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang.virtualized" % "scala-reflect" % _ % "provided"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang.virtualized" % "scala-compiler" % _ % "provided"),
    libraryDependencies ++= Seq("com.huawei.scalan" %% "scalan-common" % "0.2.9-SNAPSHOT"),
    libraryDependencies ++= Seq("com.huawei.scalan" %% "scalan-core" % "0.2.9-SNAPSHOT"),
    libraryDependencies ++= Seq("com.huawei.scalan" %% "scalan-meta" % "0.2.9-SNAPSHOT" exclude("org.scala-lang", "scala-compiler")),
    publishArtifact in (Compile, packageBin) := false,
    assemblyOption in assembly ~= { _.copy(includeScala = false, includeDependency = true) },
    artifact in (Compile, assembly) := {
      val art = (artifact in (Compile, assembly)).value
      art.copy(`classifier` = Some("assembly"))
    },
    addArtifact(artifact in (Compile, assembly), assembly)
  )
}
