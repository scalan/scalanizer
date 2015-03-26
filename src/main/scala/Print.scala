package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

/** The component outputs the tree of compilation unit. */
class Printer(val global: Global) extends PluginComponent {
  import global._

  val phaseName: String = "scalan-print"
  override def description: String = "Print AST of compilation units"

  val runsAfter = List[String]("scalan-annotation")
  override val runsRightAfter: Option[String] = Some("scalan-annotation")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      if (unit.source.file.name == "Segs.scala")
        print(unit.body)
    }
  }
}
