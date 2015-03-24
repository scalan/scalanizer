package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.reflect.api.Quasiquotes

/** The class imports Scalan framework particular packages. */
class ScalanImport(val global: Global) extends PluginComponent with Transform  {
  import global._

  val phaseName: String = "scalan-import"

  val runsAfter = List[String]("parser")
  override val runsRightAfter: Option[String] = Some("parser")

  def newTransformer(unit: CompilationUnit) = new ScalanImporter

  class ScalanImporter extends Transformer {
    override def transform(tree: Tree): Tree = {
      tree match {
        case PackageDef(segs @ Ident(TermName("segs")), stats: List[Tree]) =>
          val imports = List[Tree](
            q"import scalan._",
            q"import scalan.paradise._"
          )
          val traits = List[Tree](
            q"trait SegsDsl extends impl.SegsAbs",
            q"trait SegsDslSeq extends impl.SegsSeq",
            q"trait SegsDslExp extends impl.SegsExp"
          )
          val newstats = imports ++ stats ++ traits
          print(newstats)
          PackageDef(segs, newstats)
        case _ => tree
      }
    }
  }
}
