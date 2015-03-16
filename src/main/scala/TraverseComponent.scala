package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

/** The component finds functions. */
class TraverseComponent(val global: Global) extends PluginComponent {
  import global._

  val phaseName: String = "scalan-fun"
  val runsAfter = List[String]("refchecks")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      newTraverser().traverse(unit.body)
    }
  }

  def newTraverser(): Traverser = new ForeachTreeTraverser(check)

  def check(tree: Tree): Unit = tree match {
    case Apply(fun, args) => println("Func: "+ fun)
    case _ => ()
  }
}
