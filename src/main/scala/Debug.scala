package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

object Debug {
  val name = "scalan-debug"
}

/** The component outputs the tree of compilation unit. */
class Debug(plugin: ScalanPlugin) extends ScalanizerComponent(plugin) {
  import global._

  val phaseName: String = Debug.name
  override def description: String = "Print AST of compilation units"

  val runsAfter = List(FinalComponent.name)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {

      if (ScalanPluginConfig.codegenConfig.entityFiles.contains(unit.source.file.name)) {
        print(showCode(unit.body))
      }
    }
  }
}
