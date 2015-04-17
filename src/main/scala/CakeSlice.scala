package scalan.plugin

import scala.tools.nsc._
import ScalanAst._

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

  def genParents(ancestors: List[STraitCall]): List[Tree] = {
    val parents = Select(Ident("scala"), TypeName("AnyRef"))

    parents :: ancestors.map{ancestor =>
      val tpt = TypeName(ancestor.name)
      val tpts = ancestor.tpeSExprs.map(_ match {
        case tpe: STraitCall => genTypeByName(tpe.name)
      })

      tq"$tpt[..$tpts]"
    }
  }

  def genEntity(entity: STraitDef): Tree = {
    val entityName = TypeName(entity.name)
    val entitySelf = entity.selfType match {
      case Some(selfDef: SSelfTypeDef) => q"val ${selfDef.name}: ${genTypeByName(selfDef.tpe)}"
      case None => noSelfType
    }
    val repStats = toRepStats(entity.bodyTree.asInstanceOf[List[Tree]])
    val entityParents = genParents(entity.ancestors)
    val res = q"trait $entityName extends ..$entityParents { $entitySelf => ..$repStats }"

    //print(showRaw(res))
    res
  }

  def toRep(module: SEntityModuleDef, tree: Tree): Tree = tree match {
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
      //print(showRaw(res))
      res
    case _ => EmptyTree
  }

  def genTypeByName(name: String) = tq"${TypeName(name)}"

  def genDefaultElem(module: SEntityModuleDef): Tree = {
    val entityName = module.entityOps.name
    val entityNameType = genTypeByName(entityName)
    val defaultClassName = module.concreteSClasses.head.name
    val defaultClass = tq"${TypeName(defaultClassName)}"

    val methodName = TermName("default" + entityName + "Elem")
    val returnType = tq"Elem[$entityNameType]"

    val defaultElem = q"""
      implicit def $methodName: $returnType = element[$defaultClass].asElem[$entityNameType]
      """

    defaultElem
  }

  def genSelf(module: SEntityModuleDef): Tree = {
    val selfType = genTypeByName(module.name + "Dsl")
    val res = q"val self: $selfType"

    res
  }

  def genCompanion(comp: Option[STraitOrClassDef]): Tree = comp match {
    case Some(c) =>
      val compName = TypeName(c.name)
      q"trait $compName"
    case None => EmptyTree
  }

  def genCompanions(module: SEntityModuleDef): List[Tree] = {
    genCompanion(module.entityOps.companion) :: module.concreteSClasses.map(clazz => genCompanion(clazz.companion))
  }

  def toCake(module: SEntityModuleDef, tree: Tree): Tree = {

    tree match {
      case q"""$mods trait $tpname[..$tparams]
               extends { ..$earlydefns } with ..$parents
               { $self => ..$stats }
             """
      =>
        val newstats = genDefaultElem(module) ::
                       genEntity(module.entityOps) ::
                       (stats.map(toRep(module, _)) ++ genCompanions(module))
        val newSelf = genSelf(module)
        val name = TypeName(module.name)

        val res = q"trait $name extends Base with BaseTypes { $newSelf => ..$newstats }"
        //print(showCode(res))
        res
      case _ => tree
    }
  }

  def cookCakeSlice(module: SEntityModuleDef, orig: Tree): Tree = {
    orig.duplicate match {
      case PackageDef(pkgname, topstats) => PackageDef(pkgname, topstats.map(toCake(module, _)))
      case _ => orig
    }
  }
}
