package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.reflect.api.Quasiquotes

/** The class imports Scalan framework particular packages. */
class ScalanImport(val global: Global) extends PluginComponent with Transform  {
  import global._

  val phaseName: String = "scalan-import"
  override def description: String = "Importing of packages needed by Scalan"

  val runsAfter = List[String]("parser")
  override val runsRightAfter: Option[String] = Some("parser")

  def newTransformer(unit: CompilationUnit) = {
    if (unit.source.file.name == "Segms.scala")
      new ScalanImporter(unit.source.file.name)
    else noopTransformer
  }

  class ScalanImporter(unitName: String) extends Transformer {
    override def transform(tree: Tree): Tree = {
      tree match {
        case PackageDef(segs @ Ident(TermName("segms")), stats: List[Tree]) =>
          val imports = List[Tree](
            q"import scalan._",
            q"import scalan.paradise._"
          )
          val traits = List[Tree](
            q"trait SegmsDsl extends impl.SegmsAbs",
            q"trait SegmsDslSeq extends impl.SegmsSeq",
            q"trait SegmsDslExp extends impl.SegmsExp"
          )
          val newstats = imports ++ stats ++ traits

          PackageDef(segs, newstats)
        case _ => tree
      }
    }
  }
}
