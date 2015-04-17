package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

/** The component outputs the tree of compilation unit. */
class Debug(val global: Global) extends PluginComponent {
  import global._

  val phaseName: String = "scalan-debug"
  override def description: String = "Print AST of compilation units"

  val runsAfter = List[String]("scalan-annot")
  override val runsRightAfter: Option[String] = Some("scalan-annot")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {

      if (ScalanPluginConfig.files.contains(unit.source.file.name)) {
        print(showCode(unit.body))
      }
    }
  }
}
