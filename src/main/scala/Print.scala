package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

/** The component the tree of compilation unit. */
class Printer(val global: Global) extends PluginComponent {
  import global._

  val phaseName: String = "scalan-print"
  val runsAfter = List[String]("scalan-annotation")
  override val runsRightAfter: Option[String] = Some("scalan-annotation")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      if (unit.source.file.name == "Segs.scala")
        print(unit.body)
    }
  }
}
