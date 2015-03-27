package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.collection.mutable.Map

/** The class adds default extensions */
class DslExtension(val global: Global, emap: Map[String, Set[String]])
  extends PluginComponent with Transform  {
  import global._

  val phaseName: String = "scalan-ext"
  override def description: String = "Adding of DSL extensions"

  val runsAfter = List[String]("scalan-annot")
  override val runsRightAfter: Option[String] = Some("scalan-annot")

  def newTransformer(unit: CompilationUnit) = new DslExtender

  class DslExtender extends Transformer {
    def getTraitName(tpname: TypeName): String = tpname match {
      case TypeName(name) => name
      case _ => ""
    }

    def getTraitTrees(names: Option[Set[String]]): List[Tree] = {
      //print("Bingo")
      List[Tree](
        q"trait SegmsDsl extends impl.SegmsAbs",
        q"trait SegmsDslSeq extends impl.SegmsSeq",
        q"trait SegmsDslExp extends impl.SegmsExp"
      )
    }

    def isCakeSlice(mods: Modifiers): Boolean = {
      mods.annotations.exists(annot => annot match {
        case Apply(Select(New(Ident(TypeName("CakeSlice"))), _), _) => true
        case _ => false
      })
    }

    override def transform(tree: Tree): Tree = tree match {
      case PackageDef(pkgname, pkgstats: List[Tree]) =>
        val tstats = pkgstats.flatMap(stat => stat match {
          case q"""$mods trait $tpname[..$tparams]
                    extends { ..$earlydefns } with ..$parents
                    { $self => ..$stats }
                 """ if isCakeSlice(mods) =>
            val traitnames = emap get getTraitName(tpname)

            getTraitTrees(traitnames)
          case _ => List[Tree]()
        })

        PackageDef(pkgname, pkgstats ++ tstats)
      case _ => tree
    }
  }
}

