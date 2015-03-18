package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform

/** The class adds annotations to objects. */
class AddAnnot(val global: Global) extends PluginComponent with Transform  {
  import global._

  val phaseName: String = "scalan-add-annot"

  val runsAfter = List[String]("parser")
  override val runsRightAfter: Option[String] = Some("parser")

  def newTransformer(unit: CompilationUnit) = new AnnotTransformer

  class AnnotTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case PackageDef(pid: RefTree, stats: List[Tree]) => stats.head match {
        case ModuleDef(
          Modifiers(flags: Long, privateWithin: Name, annotations: List[Tree]),
          name: TermName,
          impl: Template
        ) =>
          print ("Adding of the annotation: hello to " + name)
          val annotName: Name = newTypeName ("hello")
          val annotContr: Name = newTermName ("<init>")
          val helloAnnot = Apply(Select(New(Ident(annotName)), annotContr), List[Tree]())

          val res = PackageDef (
            pid,
            List[Tree] (
              ModuleDef (
                Modifiers (
                  flags, privateWithin,
                  annotations ++ List[Tree] (helloAnnot)
                ),
                name, impl
              )
            )
          )
          res
        case _ => tree
      }
      case _ => tree
    }
  }
}
