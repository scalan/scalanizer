package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

/** The component builds wrappers. */
class Wrapping(val global: Global) extends PluginComponent {
  import global._

  val phaseName: String = "scalan-wrapping"
  override def description: String = "Building wrappers for external types"

  val runsAfter = List[String]("typer")
  override val runsRightAfter: Option[String] = Some("typer")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      if (Set("Cols.scala").contains(unit.source.file.name)) {
//        print(showRaw(unit.body, printTypes = Some(true)))
        newTraverser().traverse(unit.body)
      }
    }
  }

  def newTraverser(): Traverser = new ForeachTreeTraverser(handleMethodCalls)

  def handleMethodCalls(tree: Tree): Unit = tree match {
    case Select(objSel @ Select(_, obj), method) if isWrapper(objSel.tpe.typeSymbol) =>
      print(s"$method should be added to wrapper of ${objSel.tpe.typeSymbol}")
    case _ => ()
  }

  def isWrapper(sym: Symbol): Boolean = {
    Set("Arr").contains(sym.nameString)
  }
}

