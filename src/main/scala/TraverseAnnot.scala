package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent

/** The component finds annotations. */
class TraverseAnnot(val global: Global) extends PluginComponent {
  import global._

  val phaseName: String = "scalan-annot"
  val runsAfter = List[String]("parser")
  override val runsRightAfter: Option[String] = Some("parser")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      newTraverser().traverse(unit.body)
    }
  }

  def newTraverser(): Traverser = new ForeachTreeTraverser(out)

  def out(tree: Tree): Unit = tree match {
    case PackageDef(pid: RefTree, stats: List[Tree]) => {
      print("Package: " + pid.name + " stats = " + stats.length)
      if (stats.nonEmpty) {
        stats.head match {
          case ModuleDef(Modifiers(_, _, annotations: List[Tree]), _, _) if annotations.nonEmpty => {
            print("Annotations: len = " + annotations.length)
            annotations.head match {
              case Apply(Select(New(Ident(name0: Name)), name1: Name), args: List[Tree]) => {
                print("Annotation: " + name0)
              }
              case _ => ()
            }
          }
          case _ => ()
        }
      }
      else ()
    }
    case _ => () //print(tree.symbol + " : " + tree.summaryString + " - " + tree)
  }
}
