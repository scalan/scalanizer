package scalan.plugin

import scala.reflect.internal.util.BatchSourceFile
import scalan.meta.ScalanAst._

trait Backend extends Common with PatternMatching {

  type Compiler <: scala.tools.nsc.Global
  val compiler: Compiler
  import compiler._

  def genBoilerplate(module: SEntityModuleDef): Tree = {
    val entityGen = new scalan.meta.ScalanCodegen.EntityFileGenerator(
      module, ScalanPluginConfig.codegenConfig)
    val implCode = entityGen.getImplFile
    val implCodeFile = new BatchSourceFile("<impl>", implCode)
    val boilerplate = newUnitParser(new CompilationUnit(implCodeFile)).parse()

    boilerplate
  }

  case class GenCtx(val module: SEntityModuleDef, val toRep: Boolean = true)

  def genScalaAst(module: SEntityModuleDef, orig: Tree): Tree = orig match {
    case q"package $ref { ..$body }" =>
      implicit val ctx = GenCtx(module, true)
      val virtBody = List[Tree](genModule(module))

      q"package $ref { ..$virtBody }"
    case tree => throw new IllegalArgumentException("Module must be in a package")
  }

  def genModule(module: SEntityModuleDef)(implicit ctx: GenCtx): Tree = {
    val methods = module.methods.map(m => genMethod(m)(ctx.copy(toRep = !m.isElemOrCont)))
    val newstats =  genEntity(module.entityOps) ::
      (genConcreteClasses(module.concreteSClasses) ++ genCompanions(module) ++ methods)
    val newSelf = genModuleSelf(module)
    val name = TypeName(module.name)
    val moduleParents = genParents(module.ancestors)
    val res = q"trait $name extends ..$moduleParents { $newSelf => ..$newstats }"

    res
  }

  def genTrait(tr: STraitDef)(implicit ctx: GenCtx): Tree = {
    val entityName = TypeName(tr.name)
    val entitySelf = genSelf(tr.selfType)
    val repStats = genBody(tr.body)
    val entityParents = genParents(tr.ancestors)
    val tparams = tr.tpeArgs.map(genTypeArg)
    val mods = Modifiers(NoFlags, tpnme.EMPTY, genAnnotations(tr.annotations))
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
    val mods = Modifiers(flags, tpnme.EMPTY, genAnnotations(c.annotations))
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
    case Some(comp) => comp match {
      case t: STraitDef => genTrait(t)
      case c: SClassDef => genClass(c)
      case _ => throw new NotImplementedError(s"genCompanion: $comp")
    }
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
    val mods = Modifiers(flags, tpnme.EMPTY, genAnnotations(m.annotations))
    val tpt = m.tpeRes match {
      case Some(tpeRes) => if (ctx.toRep) repTypeExpr(tpeRes) else genTypeExpr(tpeRes)
      case None => EmptyTree
    }
    val paramss = genMethodArgs(m.argSections).filter(!_.isEmpty)
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
    val tpt = if (arg.isElemOrCont) genTypeExpr(arg.tpe) else repTypeExpr(arg.tpe)
    val overFlag = if (arg.overFlag) Flag.OVERRIDE else NoFlags
    val impFlag = if (arg.impFlag) Flag.IMPLICIT else NoFlags
    val flags = overFlag | impFlag
    val mods = Modifiers(flags, tpnme.EMPTY, genAnnotations(arg.annotations))

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
      val tpts = ancestor.tpeSExprs.map(genTypeExpr)

      tq"$tpt[..$tpts]"
    }
  }

  def genSelf(selfType: Option[SSelfTypeDef])(implicit ctx: GenCtx) = selfType match {
    case Some(SSelfTypeDef(name, Nil)) =>
      val flags = Flag.PRIVATE | Flag.LAZY
      val mods = Modifiers(NoFlags, tpnme.EMPTY, List())
      ValDef(mods, TermName(name), EmptyTree, EmptyTree)
    case Some(selfDef: SSelfTypeDef) => q"val self: ${genTypeByName(selfDef.tpe)}"
    case None => noSelfType
  }

  def genModuleSelf(module: SEntityModuleDef)(implicit ctx: GenCtx): Tree = {
    val tpeName = module.selfType match {
      case Some(st) if !st.components.isEmpty => st.components.head.name
      case _ => module.name + "Dsl"
    }
    val selfType = genTypeByName(tpeName)
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
    case _ => throw new NotImplementedError(s"genTypeExpr($tpeExpr)")
  }

  def repTypeExpr(tpeExpr: STpeExpr)(implicit ctx: GenCtx) = tpeExpr match {
    case STpePrimitive(name: String, _) => tq"Rep[${TypeName(name)}]"
    case STraitCall(name: String, tpeSExprs: List[STpeExpr]) =>
      val targs = tpeSExprs.map(genTypeExpr)
      val appliedType = tq"${TypeName(name)}[..$targs]"
      if (ScalanPluginConfig.typeClasses.contains(name))
        appliedType
      else
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
    val mods = Modifiers(flags, tpnme.EMPTY, genAnnotations(arg.annotations))

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
    val constrName = constr.name.split('.').last.split('[').head

    Apply(Ident(TermName(constrName)), argsTree)
  }

  def genFunc(func: SFunc)(implicit ctx: GenCtx): Tree = {
    if (func.params.length == 1) {
      q"fun { (${genVal(func.params.head)}) => ${genExpr(func.res)} }"
    } else {
      val t: List[STpeExpr] = func.params.map(_.tpe.getOrElse(STpeEmpty()))
      val tAst = genTuples(t)
      val (_, vals) = func.params.foldLeft((1, List[Tree]())) { (acc, param) =>
        val tres = param.tpe match {
          case Some(tpe) => repTypeExpr(tpe)
          case None => TypeTree()
        }
        val inval = q"in.${TermName("_" + acc._1.toString())}"
        (acc._1 + 1, q"val ${TermName(param.name)}: $tres = $inval" :: acc._2)
      }
      val body = q"{ ..${vals.reverse}; ${genExpr(func.res)} }"
      q"fun { (in: Rep[$tAst]) => $body }"
    }
  }

  def genExpr(expr: SExpr)(implicit ctx: GenCtx): Tree = expr match {
    case SEmpty() => q""
    case SConst(c) =>
      val constTree = compiler.Literal(compiler.Constant(c))
      if (ctx.toRep) q"toRep($constTree)" else constTree
    case SIdent(name: String) => Ident(TermName(name))
    case SAssign(left, right) => q"${genExpr(left)} = ${genExpr(right)}"
    case SApply(SSelect(SAscr(obj, STraitCall("Array", _)), method), _, argss) => genBasisArray(obj, method, argss)
    case SSelect(SAscr(obj, STraitCall("Array", _)), method) => genBasisArray(obj, method, List(List()))
    case SApply(SSelect(obj @ SIdent("Array"), method), _, argss) => genBasisArray(obj, method, argss)
    case SSelect(expr: SExpr, tname: String) => q"${genExpr(expr)}.${TermName(tname)}"
    case SApply(fun, tpts, argss) =>
      val typeArgs = tpts.map(genTypeExpr)
      val valArgss = argss.map(_.map(genExpr))
      fun match {
        case SSelect(SIdent(pkg), name) if pkg == "scala" && name.startsWith("Tuple") =>
          q"Tuple[..$typeArgs](...$valArgss)"
        case _ => q"${genExpr(fun)}[..$typeArgs](...$valArgss)"
      }
    case SBlock(init: List[SExpr], last) => Block(init.map(genExpr), genExpr(last))
    case SIf(c, t, e) => q"IF (${genExpr(c)}) THEN {${genExpr(t)}} ELSE {${genExpr(e)}}"
    case SAscr(expr, tpt) => q"${genExpr(expr)}: ${repTypeExpr(tpt)}"
    case constr: SContr => genConstr(constr)
    case func: SFunc => genFunc(func)
    case SThis(tname) => q"${TypeName(tname)}.this"
    case SSuper(name, qual, field) => q"${TypeName(name)}.super[${TypeName(qual)}].${TermName(field)}"
    case SAnnotated(expr, annot) => q"${genExpr(expr)}: @${TypeName(annot)}"
    case STypeApply(fun, tpts) => q"${genExpr(fun)}[..${tpts.map(genTypeExpr)}]"
    case STuple(exprs) => q"Tuple(..${exprs.map(genExpr)})"
    case m: SMatch => genExpr(transformPatternMatching(m))
    case bi: SBodyItem => genBodyItem(bi)
    case unknown => throw new NotImplementedError(s"genExpr($unknown)")
  }

  def genAnnotations(annotations: List[SAnnotation])(implicit ctx: GenCtx): List[Tree] = {
    annotations.filterNot(isInternalAnnot).map(genAnnotation)
  }
  def genAnnotation(annot: SAnnotation)(implicit ctx: GenCtx): Tree = {
    def genAnnotExpr(expr: SExpr): Tree = expr match {
      case SConst(c: Any) => compiler.Literal(compiler.Constant(c))
      case SIdent(name: String) => Ident(TermName(name))
      case SAssign(left, right) => q"${genAnnotExpr(left)} = ${genAnnotExpr(right)}"
      case unknown => throw new NotImplementedError(s"genAnnotExpr($unknown)")
    }
    val args = annot.args.map(genAnnotExpr)
    Apply(Select(New(Ident(annot.annotationClass)), nme.CONSTRUCTOR), args)
  }

  def genBasisArray(obj: SExpr, method: String, argss: List[List[SExpr]])
                   (implicit ctx: GenCtx): Tree = {
    val constArgs = genExpr(obj) :: argss.flatten.map(genExpr)
    method match {
      case "map" => q"array_map(..${constArgs.take(2)})"
      case "length" => q"array_length(..${constArgs.take(1)})"
      case "apply" => q"array_apply(..${constArgs.take(2)})"
      case "reduce" =>
        val plus = Literal(Constant("+"))
        val SSelect(monoidName, _) = argss.head.head
        q"array_reduce(..${constArgs.take(1)})(RepMonoid(opName = $plus, zero = ${genExpr(monoidName)}.zero, append = ${genExpr(monoidName)}.append, isCommutative = true))"
      case "range" =>
        val List(_,_,end,_) = constArgs
        q"SArray.rangeFrom0($end)"
    }
  }
}
