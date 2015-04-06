package scalan.plugin

import scala.tools.nsc._

trait CakeSlice { self: ScalanPluginCake =>
  val global: Global
  import global._

  private def defaultElem(stats: List[Tree]) = {

    val defaultElem = q"implicit def defaultSegmElem: Elem[Segm] = element[Interval].asElem[Segm]"
    stats :+ defaultElem
  }

  private def toCake(tree: Tree): Tree = {

    tree match {
      case q"""$mods trait $tpname[..$tparams]
               extends { ..$earlydefns } with ..$parents
               { $self => ..$stats }
             """
      =>
        val newstats = defaultElem(stats)
        val res =
          q"""
            $mods trait $tpname[..$tparams]
            extends { ..$earlydefns } with ..$parents with Base with BaseTypes
               { self: SegmsDsl => ..$newstats }
            """
        //print(showCode(res))
        res
      case _ => tree
    }
  }

  def cookCakeSlice(orig: Tree): Tree = orig match {
    case PackageDef(pkgname, topstats) => PackageDef(pkgname, topstats.map(toCake(_)))
    case _ => orig
  }
}
