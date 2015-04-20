package scalan.plugin

import scala.tools.nsc._
import ScalanAst._

trait GenScalaAst { self: ScalanPluginCake =>
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

  def toRepStats(body: List[SBodyItem], stats: List[Tree]): List[Tree] = {
    //print(showRaw(stats))
    val repStats = stats.map((stat: Tree) => stat match {
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

    repStats
  }

  def genMethodArg(arg: SMethodArg): Tree = {
    val tname = TermName(arg.name)
    val tpt = repTypeExpr(arg.tpe)
    val overFlag = if (arg.overFlag) Flag.OVERRIDE else NoFlags
    val impFlag = if (arg.impFlag) Flag.IMPLICIT else NoFlags
    val flags = overFlag | impFlag
    val mods = Modifiers(flags)

    q"$mods val $tname: $tpt"
  }

  def genMethodArgs(argSections: List[SMethodArgs]): List[List[Tree]] = {
    argSections.map(_.args.map(genMethodArg))
  }

  def genBody(body: List[SBodyItem]): List[Tree] = {
    val repBody = body.map((item: SBodyItem) => item match {
      case m: SMethodDef =>
        val tname = TermName(m.name)
        val impFlag = if (m.isImplicit) Flag.IMPLICIT else NoFlags
        val flags = Flag.PARAM | impFlag
        val mods = Modifiers(flags)
        val reptpt = m.tpeRes match {
          case Some(tpeRes) => repTypeExpr(tpeRes)
          case None => EmptyTree
        }
        val repparamss = genMethodArgs(m.argSections)

        q"$mods def $tname(...$repparamss): $reptpt"
      case _ => print("Unsupported body item: " + item);EmptyTree
    })

    repBody
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

  def genSelf(selfType: Option[SSelfTypeDef]) = selfType match {
    case Some(selfDef: SSelfTypeDef) => q"val ${selfDef.name}: ${genTypeByName(selfDef.tpe)}"
    case None => noSelfType
  }

  def genEntity(entity: STraitDef): Tree = {
    val entityName = TypeName(entity.name)
    val entitySelf = genSelf(entity.selfType)
    val repStats = genBody(entity.body)
    val entityParents = genParents(entity.ancestors)
    val res = q"trait $entityName extends ..$entityParents { $entitySelf => ..$repStats }"

    //print(showRaw(res))
    res
  }

  def genTypeExpr(tpeExpr: STpeExpr): Tree = tpeExpr match {
    case STpePrimitive(name: String, _) => tq"${TypeName(name)}"
    case STraitCall(name: String, tpeSExprs: List[STpeExpr]) =>
      val targs = tpeSExprs.map(genTypeExpr)
      tq"${TypeName(name)}[..$targs]"
  }

  def repTypeExpr(tpeExpr: STpeExpr) = tpeExpr match {
    case STpePrimitive(name: String, _) => tq"Rep[${TypeName(name)}]"
    case STraitCall(name: String, tpeSExprs: List[STpeExpr]) =>
      val targs = tpeSExprs.map(genTypeExpr)
      val appliedType = tq"${TypeName(name)}[..$targs]"
      tq"Rep[$appliedType]"
    case _ => print("Unsupported tpeEpr: " + tpeExpr); EmptyTree
  }

  def genClassArg(arg: SClassArg): Tree = {
    val tname = TermName(arg.name)
    val tpt = repTypeExpr(arg.tpe)
    val valFlag = if (arg.valFlag) Flag.PARAMACCESSOR else NoFlags
    val overFlag = if (arg.overFlag) Flag.OVERRIDE else NoFlags
    val impFlag = if (arg.impFlag) Flag.IMPLICIT else NoFlags
    val flags = Flag.PARAM | valFlag | overFlag | impFlag
    val mods = Modifiers(flags)

    q"$mods val $tname: $tpt"
  }

  def genClassArgs(args: SClassArgs, implicitArgs: SClassArgs): List[List[Tree]] = {
    val repArgs = args.args.map(genClassArg)
    val repImplArgs = implicitArgs.args.map(genClassArg)
    val repClassArgs = List[List[Tree]](repArgs, repImplArgs)

    repClassArgs.filterNot(_.isEmpty)
  }

  def genClasses(classes: List[SClassDef]): List[Tree] = {
    classes.map{clazz =>
      val className = TypeName(clazz.name)
      val classSelf = genSelf(clazz.selfType)
      val parents = genParents(clazz.ancestors)
      val repStats = toRepStats(clazz.body, clazz.bodyTree.asInstanceOf[List[Tree]].filter(_ match {
        case dd: DefDef if dd.name == TermName(termNames.CONSTRUCTOR) => false
        case _ => true
      }))
      val repparamss = genClassArgs(clazz.args, clazz.implicitArgs)
      val res = q"""
            abstract class $className (...$repparamss)
            extends ..$parents
            { $classSelf => ..$repStats }
            """
      //print(showRaw())
      res
    }
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

  def genModuleSelf(module: SEntityModuleDef): Tree = {
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

  def genModule(module: SEntityModuleDef): Tree = {
    val newstats = genDefaultElem(module) ::
      genEntity(module.entityOps) ::
      (genClasses(module.concreteSClasses) ++ genCompanions(module))
    val newSelf = genModuleSelf(module)
    val name = TypeName(module.name)

    val res = q"trait $name extends Base with BaseTypes { $newSelf => ..$newstats }"
    val classes = genClasses(module.concreteSClasses)
    //print(showCode(res))

    res
  }

  def genScalaAst(module: SEntityModuleDef, orig: Tree): Tree = orig match {
    case q"package $ref { ..$body }" =>
      val virtBody = List[Tree](genModule(module))

      q"package $ref { ..$virtBody }"
    case tree => ???(tree)
  }
}
