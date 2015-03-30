package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

/** The component reads META boilerplate from source files. */
class ReadMeta(val global: Global) extends PluginComponent {
  import global._

  val phaseName: String = "scalan-imeta"
  override def description: String = "Read META boilerplate"

  val runsAfter = List[String]("scalan-ext")
  override val runsRightAfter: Option[String] = Some("scalan-ext")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      if ("SegmsImpl.scala" == unit.source.file.name) {
        val srcFile = getSourceFile(unit.source.file.path)
        val cu = new CompilationUnit(srcFile)
        val newBody = newUnitParser(cu).parse()

        //print("Path: " + unit.source.file.path)
        //print("Name: " + unit.source.file.name)

        unit.body = newBody
      }
    }
  }
}

