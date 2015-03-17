package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.InfoTransform

/** The class adds annotations to objects. */
class AddAnnot(val global: Global) extends PluginComponent with InfoTransform  {
  import global._

  val phaseName: String = "scalan-add-annot"

  val runsAfter = List[String]("parser")
  override val runsRightAfter: Option[String] = Some("parser")

  def transformInfo(sym: Symbol, tp: Type): Type = infoTransformer.mapOver(tp)

  private val infoTransformer = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case MethodType(pts, rt) =>
        println("methodType (_, _, ..) => "+ rt)
        tp
      case _ => mapOver(tp)
    }
  }

  def newTransformer(unit: CompilationUnit) = new Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case ModuleDef(
             Modifiers(flags: Long, privateWithin: Name, annotations: List[Tree]),
             name: TermName,
             impl: Template
      ) => {
        val name0: Name = ???
        val name1: Name = ???
        val helloAnnot = Apply(Select(New(Ident(name0)), name1), List[Tree]())

        ModuleDef(Modifiers(flags, privateWithin, annotations), name, impl)
      }
    }
  }
}
