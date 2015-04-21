package scalan.plugin

import scala.tools.nsc._
import ScalanAst._

trait GenScalaAst { self: ScalanPluginCake =>
  val global: Global
  import global._

  def genScalaAst(module: SEntityModuleDef, orig: Tree): Tree = orig match {
    case q"package $ref { ..$body }" =>
      val virtBody = List[Tree](genModule(module))

      q"package $ref { ..$virtBody }"
    case tree => ???(tree)
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

  def genEntity(entity: STraitDef): Tree = {
    val entityName = TypeName(entity.name)
    val entitySelf = genSelf(entity.selfType)
    val repStats = genBody(entity.body)
    val entityParents = genParents(entity.ancestors)
    val res = q"trait $entityName extends ..$entityParents { $entitySelf => ..$repStats }"

    //print(showRaw(res))
    res
  }

  def genClasses(classes: List[SClassDef]): List[Tree] = {
    classes.map{clazz =>
      val className = TypeName(clazz.name)
      val classSelf = genSelf(clazz.selfType)
      val parents = genParents(clazz.ancestors)
      val repStats = genBody(clazz.body)
      val repparamss = genClassArgs(clazz.args, clazz.implicitArgs)
      val res = q"""
            abstract class $className (...$repparamss)
            extends ..$parents
            { $classSelf => ..$repStats }
            """
      res
    }
  }

  def genCompanions(module: SEntityModuleDef): List[Tree] = {
    genCompanion(module.entityOps.companion) :: module.concreteSClasses.map(clazz => genCompanion(clazz.companion))
  }

  def genCompanion(comp: Option[STraitOrClassDef]): Tree = comp match {
    case Some(c) =>
      val compName = TypeName(c.name)
      q"trait $compName"
    case None => EmptyTree
  }

  def genBody(body: List[SBodyItem]): List[Tree] = {
    val repBody = body.map((item: SBodyItem) => item match {
      case m: SMethodDef => genMethod(m)
      case v: SValDef => genVal(v)
      case i: SImportStat => genImport(i)
      case _ => print("Unsupported body item: " + item);EmptyTree
    })

    repBody
  }

  def genMethod(m: SMethodDef): Tree = {
    val tname = TermName(m.name)
    val impFlag = if (m.isImplicit) Flag.IMPLICIT else NoFlags
    val flags = Flag.PARAM | impFlag
    val mods = Modifiers(flags)
    val tpt = m.tpeRes match {
      case Some(tpeRes) => repTypeExpr(tpeRes)
      case None => EmptyTree
    }
    val paramss = genMethodArgs(m.argSections)
    val exprs = m.body match {
      case Some(SExternalExpr(tree)) => toRepExpr(tree.asInstanceOf[Tree])
      case None => EmptyTree
    }

    q"$mods def $tname(...$paramss): $tpt = $exprs"
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

  def genVal(v: SValDef): Tree = {
    val impFlag = if (v.isImplicit) Flag.IMPLICIT else NoFlags
    val lazyFlag = if (v.isLazy) Flag.LAZY else NoFlags
    val mods = Modifiers(impFlag | lazyFlag)
    val tname = TermName(v.name)
    val tpt =v.tpe match {
      case Some(tpe) => repTypeExpr(tpe)
      case None => TypeTree()
    }
    val expr = v.expr match {
      case SExternalExpr(e) => toRepExpr(e.asInstanceOf[Tree])
      case _ => print("Unsupported value initializer: " + v.expr); EmptyTree
    }

    q"$mods val $tname: $tpt = $expr"
  }

  def genRefs(refs: List[String]): Tree = {
    if (refs.length == 1)
      Ident(TermName(refs.head))
    else
      q"${genRefs(refs.init)}.${TermName(refs.last)}"
  }

  def genImport(imp: SImportStat): Tree = {
    val impParts = imp.name.split('.').toList
    val refs = genRefs(impParts.init)
    val sels = impParts.last match {
      case bindName => List(Bind(TermName(bindName), Ident(termNames.WILDCARD)))
      case "_" => List(Ident(termNames.WILDCARD))
    }

    q"import $refs.{..$sels}"
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

  def genModuleSelf(module: SEntityModuleDef): Tree = {
    val selfType = genTypeByName(module.name + "Dsl")
    val res = q"val self: $selfType"

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
}
