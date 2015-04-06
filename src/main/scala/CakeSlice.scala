package scalan.plugin

import scala.tools.nsc._

trait CakeSlice { self: ScalanPluginCake =>
  val global: Global
  import global._

  def toRepType(tp: Tree): Tree = tp match {
    case tq"" => tp
    case tq"$tpname" => tq"Rep[$tpname]"
  }

  def toRepExpr(expr: Tree): Tree = expr match {
    case q"$expr: $tpt" =>
      val reptpt = toRepType(tpt)
      q"$expr: $reptpt"
    case q"if ($cond) $thenexpr else $elseexpr" =>
      q"IF ($cond) THEN {$thenexpr} ELSE {$elseexpr}"
    case q"${global.Literal(Constant(c))}" => q"toRep(${global.Literal(Constant(c))})"
    case Apply(Select(New(tpt), termNames.CONSTRUCTOR), args) =>
      val Ident(TypeName(name)) = tpt
      Apply(Ident(TermName(name)), args)
    case _ =>
      //print(showRaw(expr))
      expr
  }

  def toRepParam(param: Tree): Tree = param match {
    case q"$mods val $name: $tpt = $rhs" =>
      val reptpt = toRepType(tpt)
      val reprhs = toRepExpr(rhs)

      q"$mods val $name: $reptpt = $reprhs"
    case _ => param
  }

  def toRepStats(stats: List[Tree]): List[Tree] = {
    stats.map((stat: Tree) => stat match {
      case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
        val reptpt = toRepType(tpt)
        val repparamss = paramss.map(_.map(param => toRepParam(param)))
        val repexpr = toRepExpr(expr)

        q"$mods def $tname[..$tparams](...$repparamss): $reptpt = $repexpr"
      case q"$mods var $name: $tpt = $rhs" =>
        val reptpt = toRepType(tpt)
        val reprhs = toRepExpr(rhs)

        q"$mods var $name: $reptpt = $reprhs"
      case q"$mods val $name: $tpt = $rhs" =>
        val reptpt = toRepType(tpt)
        val reprhs = toRepExpr(rhs)

        q"$mods val $name: $reptpt = $reprhs"
      case _ => stat
    })
  }

  def toRep(tree: Tree): Tree = tree match {
    case q"""$mods trait $tpname[..$tparams]
               extends { ..$earlydefns } with ..$parents
               { $self => ..$stats }
             """
    =>
      val repStats = toRepStats(stats)
      val res = q"""
            $mods trait $tpname[..$tparams]
            extends { ..$earlydefns } with ..$parents with Reifiable[$tpname[..$tparams]]
               { $self => ..$repStats }
            """
      //print(showCode(res))
      res
    case q"""
            $mods class $tpname[..$tparams] $ctorMods(...$paramss)
            extends { ..$earlydefns } with ..$parents
            { $self => ..$stats }
            """
    =>
      val repStats = toRepStats(stats)
      val repparamss = paramss.map(_.map(param => toRepParam(param)))
      val res = q"""
            abstract class $tpname[..$tparams] $ctorMods(...$repparamss)
            extends { ..$earlydefns } with ..$parents
            { $self => ..$repStats }
            """
      //print(showCode(res))
      res
    case _ => tree
  }

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
        val newstats = defaultElem(stats).map(toRep _)
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

  def cookCakeSlice(orig: Tree): Tree = {
    orig.duplicate match {
      case PackageDef(pkgname, topstats) => PackageDef(pkgname, topstats.map(toCake(_)))
      case _ => orig
    }
  }
}
