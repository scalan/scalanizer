package scalan.plugin

import scala.tools.nsc._
import ScalanAst._
import scala.tools.nsc.util.BatchSourceFile

trait GenScalaAst { self: ScalanPluginCake =>
  val global: Global
  import global._

  case class GenCtx(val module: SEntityModuleDef, val toRep: Boolean = true)

  def genScalaAst(module: SEntityModuleDef, orig: Tree): Tree = orig match {
    case q"package $ref { ..$body }" =>
      implicit val ctx = GenCtx(module, true)
      val virtBody = List[Tree](genModule(module))

      q"package $ref { ..$virtBody }"
    case tree => throw new IllegalArgumentException("Module must be in a package")
  }

  def genModule(module: SEntityModuleDef)(implicit ctx: GenCtx): Tree = {
    val newstats = /*genDefaultElem(module) :: */
      genEntity(module.entityOps) ::
      (genConcreteClasses(module.concreteSClasses) ++ genCompanions(module))
    val newSelf = genModuleSelf(module)
    val name = TypeName(module.name)
    val res = q"trait $name extends Base { $newSelf => ..$newstats }"

    res
  }

  def genTrait(tr: STraitDef)(implicit ctx: GenCtx): Tree = {
    val entityName = TypeName(tr.name)
    val entitySelf = genSelf(tr.selfType)
    val repStats = genBody(tr.body)
    val entityParents = genParents(tr.ancestors)
    val tparams = tr.tpeArgs.map(genTypeArg)
    val mods = Modifiers(NoFlags, tpnme.EMPTY, tr.annotations.map(genAnnotation))
    val res = q"$mods trait $entityName[..$tparams] extends ..$entityParents { $entitySelf => ..$repStats }"

    res
  }

  def genEntity(entity: STraitDef)(implicit ctx: GenCtx): Tree = genTrait(entity)

  def genClass(c: SClassDef)(implicit ctx: GenCtx): Tree = {
    val className = TypeName(c.name)
    val classSelf = genSelf(c.selfType)
    val parents = genParents(c.ancestors)
    val repStats = genBody(c.body)
    val repparamss = genClassArgs(c.args, c.implicitArgs)
    val flags = if (c.isAbstract) Flag.ABSTRACT else NoFlags
    val mods = Modifiers(flags, tpnme.EMPTY, c.annotations.map(genAnnotation))
    val tparams = c.tpeArgs.map(genTypeArg)
    val res = q"""
            $mods class $className[..$tparams] (...$repparamss)
            extends ..$parents
            { $classSelf => ..$repStats }
            """
    res
  }

  def genConcreteClasses(classes: List[SClassDef])(implicit ctx: GenCtx): List[Tree] = {
    classes.map{clazz => genClass(clazz.copy(isAbstract = true))}
  }

  def genCompanions(module: SEntityModuleDef)(implicit ctx: GenCtx): List[Tree] = {
    genCompanion(module.entityOps.companion) :: module.concreteSClasses.map(clazz => genCompanion(clazz.companion))
  }

  def genCompanion(comp: Option[STraitOrClassDef])(implicit ctx: GenCtx): Tree = comp match {
    case Some(c) => q"trait ${TypeName(c.name)}"
    case None => EmptyTree
  }

  def genBodyItem(item: SBodyItem)(implicit ctx: GenCtx): Tree = item match {
    case m: SMethodDef =>
      if (m.isElemOrCont) genMethod(m)(ctx = ctx.copy(toRep = false))
      else genMethod(m)
    case v: SValDef => genVal(v)
    case i: SImportStat => genImport(i)
    case t: STpeDef => genTypeDef(t)
    case o: SObjectDef => genObject(o)
    case tr: STraitDef => genTrait(tr)
    case c: SClassDef => genClass(c)
    case unknown => throw new NotImplementedError(s"genBodyItem($unknown)")
  }

  def genBody(body: List[SBodyItem])(implicit ctx: GenCtx): List[Tree] = body.map(genBodyItem)

  def genMethod(m: SMethodDef)(implicit ctx: GenCtx): Tree = {
    val tname = TermName(m.name)
    val impFlag = if (m.isImplicit) Flag.IMPLICIT else NoFlags
    val overFlag = if (m.isOverride) Flag.OVERRIDE else NoFlags
    val flags = Flag.PARAM | impFlag | overFlag
    val mods = Modifiers(flags, tpnme.EMPTY, m.annotations.map(genAnnotation))
    val tpt = m.tpeRes match {
      case Some(tpeRes) => if (ctx.toRep) repTypeExpr(tpeRes) else genTypeExpr(tpeRes)
      case None => EmptyTree
    }
    val paramss = genMethodArgs(m.argSections)
    val exprs = m.body match {
      case Some(expr) => genExpr(expr)
      case None => EmptyTree
    }
    val tparams = genTypeArgs(m.tpeArgs)

    q"$mods def $tname[..$tparams](...$paramss): $tpt = $exprs"
  }

  def genMethodArg(arg: SMethodArg)
                  (implicit ctx: GenCtx): Tree = {
    val tname = TermName(arg.name)
    val tpt = repTypeExpr(arg.tpe)
    val overFlag = if (arg.overFlag) Flag.OVERRIDE else NoFlags
    val impFlag = if (arg.impFlag) Flag.IMPLICIT else NoFlags
    val flags = overFlag | impFlag
    val mods = Modifiers(flags, tpnme.EMPTY, arg.annotations.map(genAnnotation))

    q"$mods val $tname: $tpt"
  }

  def genMethodArgs(argSections: List[SMethodArgs])
                   (implicit ctx: GenCtx): List[List[Tree]] = {
    argSections.map(_.args.map(genMethodArg))
  }

  def genVal(v: SValDef)(implicit ctx: GenCtx): Tree = {
    val impFlag = if (v.isImplicit) Flag.IMPLICIT else NoFlags
    val lazyFlag = if (v.isLazy) Flag.LAZY else NoFlags
    val mods = Modifiers(impFlag | lazyFlag)
    val tname = TermName(v.name)
    val tpt =v.tpe match {
      case Some(tpe) => repTypeExpr(tpe)
      case None => TypeTree()
    }
    val expr = genExpr(v.expr)

    q"$mods val $tname: $tpt = $expr"
  }

  def genRefs(refs: List[String])(implicit ctx: GenCtx): Tree = {
    if (refs.length == 1)
      Ident(TermName(refs.head))
    else
      q"${genRefs(refs.init)}.${TermName(refs.last)}"
  }

  def genImport(imp: SImportStat)(implicit ctx: GenCtx): Tree = {
    val impParts = imp.name.split('.').toList
    val refs = genRefs(impParts.init)
    val sels = impParts.last match {
      case "_" => List(Ident(termNames.WILDCARD))
      case bindName => List(Bind(TermName(bindName), Ident(termNames.WILDCARD)))
    }

    q"import $refs.{..$sels}"
  }

  def genTypeDef(t: STpeDef)(implicit ctx: GenCtx): Tree = {
    val tpname = TypeName(t.name)
    val tpt = genTypeExpr(t.rhs)
    val tparams = genTypeArgs(t.tpeArgs)

    q"type $tpname[..$tparams] = $tpt"
  }

  def genTypeArgs(tpeArgs: STpeArgs)
                 (implicit ctx: GenCtx): List[TypeDef] = tpeArgs.map(genTypeArg)

  def genTypeArg(arg: STpeArg)(implicit ctx: GenCtx): TypeDef = {
    val tpname = TypeName(arg.name)
    val tparams = arg.tparams.map(genTypeArg)
    val mods = Modifiers(Flag.PARAM)
    val tpt = arg.bound match {
      case Some(tpe) => TypeBoundsTree(TypeTree(), genTypeExpr(tpe))
      case None => TypeTree()
    }

    q"$mods type $tpname[..$tparams] = $tpt"
  }

  def genObject(o: SObjectDef)(implicit ctx: GenCtx): Tree = {
    val tname = TermName(o.name)
    val parents = genParents(o.ancestors)
    val body = genBody(o.body)

    q"object $tname extends ..$parents { ..$body }"
  }

  def genParents(ancestors: List[STraitCall])(implicit ctx: GenCtx): List[Tree] = {
    val parents = Select(Ident("scala"), TypeName("AnyRef"))

    parents :: ancestors.map{ancestor =>
      val tpt = TypeName(ancestor.name)
      val tpts = ancestor.tpeSExprs.map(_ match {
        case tpe: STraitCall => genTypeExpr(tpe)
        case inval => throw new IllegalArgumentException(s"genParents($inval)")
      })

      tq"$tpt[..$tpts]"
    }
  }

  def genSelf(selfType: Option[SSelfTypeDef])(implicit ctx: GenCtx) = selfType match {
    case Some(selfDef: SSelfTypeDef) => q"val ${selfDef.name}: ${genTypeByName(selfDef.tpe)}"
    case None => noSelfType
  }

  def genModuleSelf(module: SEntityModuleDef)(implicit ctx: GenCtx): Tree = {
    val selfType = genTypeByName(module.name + "Dsl")
    val res = q"val self: $selfType"

    res
  }

  def genTypeExpr(tpeExpr: STpeExpr)(implicit ctx: GenCtx): Tree = tpeExpr match {
    case STpeEmpty() => tq""
    case STpePrimitive(name: String, _) => tq"${TypeName(name)}"
    case STraitCall(name: String, tpeSExprs: List[STpeExpr]) =>
      val targs = tpeSExprs.map(genTypeExpr)
      tq"${TypeName(name)}[..$targs]"
    case STpeTypeBounds(lo: STpeExpr, hi: STpeExpr) =>
      TypeBoundsTree(genTypeExpr(lo), genTypeExpr(hi))
    case STpeTuple(items: List[STpeExpr]) => genTuples(items)
    case STpeFunc(domain: STpeExpr, range: STpeExpr) =>
      val tpt = genTypeSel("scala", "Function1")
      val tpts = genTypeExpr(domain) :: genTypeExpr(range) :: Nil

      tq"$tpt[..$tpts]"
    case STpeSingleton(ref) => tq"${genExpr(ref)}.type"
    case STpeSelectFromTT(qualifier, name) => tq"${genTypeExpr(qualifier)}#${TypeName(name)}"
    case STpeAnnotated(tpt, annot) => tq"${genTypeExpr(tpt)} @${TypeName(annot)}"
    case STpeExistential(tpt, defns) => tq"${genTypeExpr(tpt)} forSome { ..${defns.map(genBodyItem)} }"
  }

  def repTypeExpr(tpeExpr: STpeExpr)(implicit ctx: GenCtx) = tpeExpr match {
    case STpePrimitive(name: String, _) => tq"Rep[${TypeName(name)}]"
    case STraitCall(name: String, tpeSExprs: List[STpeExpr]) =>
      val targs = tpeSExprs.map(genTypeExpr)
      val appliedType = tq"${TypeName(name)}[..$targs]"
      tq"Rep[$appliedType]"
    case STpeTuple(_) => tq"Rep[${genTypeExpr(tpeExpr)}]"
    case STpeFunc(_, _) => tq"Rep[${genTypeExpr(tpeExpr)}]"
    case unknown => throw new NotImplementedError(s"repTypeExp($unknown)")
  }

  def genClassArg(arg: SClassArg)(implicit ctx: GenCtx): Tree = {
    val tname = TermName(arg.name)
    val tpt = if (arg.isElemOrCont) genTypeExpr(arg.tpe) else repTypeExpr(arg.tpe)
    val valFlag = if (arg.valFlag) Flag.PARAMACCESSOR else NoFlags
    val overFlag = if (arg.overFlag) Flag.OVERRIDE else NoFlags
    val impFlag = if (arg.impFlag) Flag.IMPLICIT else NoFlags
    val flags = Flag.PARAM | valFlag | overFlag | impFlag
    val mods = Modifiers(flags, tpnme.EMPTY, arg.annotations.map(genAnnotation))

    q"$mods val $tname: $tpt"
  }

  def genClassArgs(args: SClassArgs, implicitArgs: SClassArgs)
                  (implicit ctx: GenCtx): List[List[Tree]] = {
    val repArgs = args.args.map(genClassArg)
    val repImplArgs = implicitArgs.args.map(genClassArg)
    val repClassArgs = List[List[Tree]](repArgs, repImplArgs)

    repClassArgs.filterNot(_.isEmpty)
  }

  def genTypeByName(name: String)(implicit ctx: GenCtx) = tq"${TypeName(name)}"

  def genDefaultElem(module: SEntityModuleDef)
                    (implicit ctx: GenCtx): Tree = {
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

  def genTypeSel(ref: String, name: String)(implicit ctx: GenCtx) = {
    Select(Ident(ref), TypeName(name))
  }

  def genTuple2(first: Tree, second: Tree)(implicit ctx: GenCtx): Tree = {
    val tpt = genTypeSel("scala", "Tuple2")
    val tpts = first :: second :: Nil

    tq"$tpt[..$tpts]"
  }

  def genTuples(elems: List[STpeExpr])(implicit ctx: GenCtx): Tree = elems match {
    case x :: y :: Nil => genTuple2(genTypeExpr(x), genTypeExpr(y))
    case x :: xs => genTuple2(genTypeExpr(x), genTuples(xs))
    case Nil => throw new IllegalArgumentException("Tuple must have at least 2 elements.")
  }

  def genConstr(constr: SContr)(implicit ctx: GenCtx): Tree = {
    val argsTree = constr.args.map(genExpr)
    if (ctx.module.concreteSClasses.exists(clazz => clazz.name == constr.name))
      Apply(Ident(TermName(constr.name)), argsTree)
    else
      throw new IllegalArgumentException(s"genConstr($constr)")
  }

  def genExpr(expr: SExpr)(implicit ctx: GenCtx): Tree = expr match {
    case SEmpty() => q""
    case SConst(c: Any) => q"toRep(${global.Literal(global.Constant(c))})"
    case SIdent(name: String) => Ident(TermName(name))
    case SAssign(left, right) => q"${genExpr(left)} = ${genExpr(right)}"
    case SSelect(expr: SExpr, tname: String) => q"${genExpr(expr)}.${TermName(tname)}"
    case SApply(fun, tpts, args) =>
      val typeArgs = tpts.map(genTypeExpr)
      val valArgs = args.map(genExpr)
      fun match {
        case SSelect(SIdent(pkg), name) if pkg == "scala" && name.startsWith("Tuple") =>
          q"Tuple[..$typeArgs](..$valArgs)"
        case _ => q"${genExpr(fun)}[..$typeArgs](..$valArgs)"
      }
    case SBlock(init: List[SExpr], last) => Block(init.map(genExpr), genExpr(last))
    case SIf(c, t, e) => q"IF (${genExpr(c)}) THEN {${genExpr(t)}} ELSE {${genExpr(e)}}"
    case SAscr(expr, tpt) => q"${genExpr(expr)}: ${repTypeExpr(tpt)}"
    case constr: SContr => genConstr(constr)
    case SFunc(params, res) => q"(..${params.map(genExpr)}) => ${genExpr(res)}"
    case SThis(tname) => q"${TypeName(tname)}.this"
    case SSuper(name, qual, field) => q"${TypeName(name)}.super[${TypeName(qual)}].${TermName(field)}"
    case SAnnotated(expr, annot) => q"${genExpr(expr)}: @${TypeName(annot)}"
    case STypeApply(fun, tpts) => q"${genExpr(fun)}[..${tpts.map(genTypeExpr)}]"
    case bi: SBodyItem => genBodyItem(bi)
    case unknown => throw new NotImplementedError(s"genExpr($unknown)")
  }

  def genAnnotation(annot: SAnnotation)(implicit ctx: GenCtx): Tree = {
    def genAnnotExpr(expr: SExpr): Tree = expr match {
      case SConst(c: Any) => global.Literal(global.Constant(c))
      case SIdent(name: String) => Ident(TermName(name))
      case SAssign(left, right) => q"${genAnnotExpr(left)} = ${genAnnotExpr(right)}"
      case unknown => throw new NotImplementedError(s"genAnnotExpr($unknown)")
    }
    val args = annot.args.map(genAnnotExpr)
    Apply(Select(New(Ident(annot.annotationClass)), nme.CONSTRUCTOR), args)
  }
}
