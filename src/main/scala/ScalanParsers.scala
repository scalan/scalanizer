package scalan.plugin

//import java.io.File

//import scala.reflect.{ClassTag,classTag}
import scala.tools.nsc._
//import scala.tools.nsc.Settings
//import scala.tools.nsc.reporters.StoreReporter
import scala.language.implicitConversions
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.OffsetPosition
import ScalanAst._

trait ScalanParsers {
  //val settings = new Settings
  //settings.embeddedDefaults(getClass.getClassLoader)
  //settings.usejavacp.value = true
  //val reporter = new StoreReporter
  //val compiler: Global = new Global(settings, reporter)
  val global: Global

  import global._
  implicit def nameToString(name: Name): String = name.toString

  implicit class OptionListOps[A](opt: Option[List[A]]) {
    def flatList: List[A] = opt.toList.flatten
  }

  private def positionString(tree: Tree) = {
    tree.pos match {
      case pos: RangePosition =>
        val path = pos.source.file.canonicalPath
        s"file $path at ${pos.line}:${pos.column} (start ${pos.point - pos.start} before, end ${pos.end - pos.point} after)"
      case pos: OffsetPosition =>
        val path = pos.source.file.canonicalPath
        s"file $path at ${pos.line}:${pos.column}"
      case pos => pos.toString
    }
  }

  def !!!(msg: String, tree: Tree) = {
    val fullMsg = s"$msg at ${positionString(tree)}"
    throw new IllegalStateException(fullMsg)
  }

  def !!!(msg: String) = {
    throw new IllegalStateException(msg)
  }

  def ???(tree: Tree) = {
    val pos = tree.pos
    val msg = s"Unhandled case in ${positionString(tree)}:\nAST: ${showRaw(tree)}\n\nCode for AST: $tree"
    throw new IllegalStateException(msg)
  }

  def parse(tree: Tree) = tree match {
    case pd: PackageDef =>
      entityModule(pd)
    case tree =>
      throw new Exception(s"Unexpected Scala tree")
  }

  //def config: CodegenConfig
/*
  def parseEntityModule(file: File) = {
    val source = compiler.getSourceFile(file.getPath)
    val tree = compiler.parseTree(source)
    tree match {
      case pd: PackageDef =>
        entityModule(pd)
      case tree =>
        throw new Exception(s"Unexpected tree in file $file:\n\n$tree")
    }
  }
*/
  def seqImplementation(methods: List[DefDef], parent: Tree): List[SMethodDef] = {
    methods.map(methodDef(_))
  }

  def entityModule(fileTree: PackageDef) = {
    val packageName = fileTree.pid.toString
    val statements = fileTree.stats
    val imports = statements.collect {
      case i: Import => importStat(i)
    }
    val moduleTraitTree = statements.collect {
      case cd: ClassDef if cd.mods.isTrait && !cd.name.contains("Dsl") => cd
    } match {
      case Seq(only) => only
      case seq => !!!(s"There must be exactly one module trait in file, found ${seq.length}")
    }
    val moduleTraitDef = traitDef(moduleTraitTree, Some(moduleTraitTree))
    val module = SEntityModuleDef(packageName, imports, moduleTraitDef/*, config*/)
    val moduleName = moduleTraitDef.name

    val dslSeq = fileTree.stats.collectFirst {
      case cd @ ClassDef(_,name,_,_) if name.toString == (moduleName + "DslSeq") => cd
    }
    val seqExplicitOps = for {
      seqImpl <- dslSeq
      seqOpsTrait <- seqImpl.impl.body.collectFirst {
        case cd @ ClassDef(_,name,_,_) if name.toString == ("Seq" + module.entityOps.name) => cd
      }
    } yield {
        val cd = seqOpsTrait.impl.body.collect { case item: DefDef => item }
        seqImplementation(cd, seqOpsTrait)
      }

    module.copy(seqDslImpl = seqExplicitOps.map(SSeqImplementation(_)))
  }

  def importStat(i: Import): SImportStat = {
    SImportStat(i.toString.stripPrefix("import "))
  }

  def isEvidenceParam(vd: ValDef) = vd.name.toString.startsWith("evidence$")

  def tpeArgs(typeParams: List[TypeDef], possibleImplicits: List[ValDef]): List[STpeArg] = {
    val evidenceTypes = possibleImplicits.filter(isEvidenceParam(_)).map(_.tpt)

    def tpeArg(tdTree: TypeDef): STpeArg = {
      val bound = tdTree.rhs match {
        case TypeBoundsTree(low, high) =>
          if (high.toString == "_root_.scala.Any")
            None
          else
            optTpeExpr(high)
        case _ => ???(tdTree)
      }
      val contextBounds = evidenceTypes.collect {
        case AppliedTypeTree(tpt, List(arg)) if arg.toString == tdTree.name.toString =>
          Some(tpt.toString)
        case _ => None
      }.flatten
      val tparams = tdTree.tparams.map(tpeArg)
      STpeArg(tdTree.name, bound, contextBounds, tparams)
    }

    typeParams.map(tpeArg)
  }

  // exclude default parent
  def ancestors(trees: List[Tree]) = trees.map(traitCall).filter(_.name != "AnyRef")

  def findCompaion(name: String, parentScope: Option[ImplDef]) = parentScope match {
    case Some(scope) => scope.impl.body.collect {
      case c: ClassDef if c.name.toString == name + "Companion" =>
        if (c.mods.isTrait) traitDef(c, parentScope) else classDef(c, parentScope)
    }.headOption
    case None => None
  }

  def traitDef(td: ClassDef, parentScope: Option[ImplDef]): STraitDef = {
    val tpeArgs = this.tpeArgs(td.tparams, Nil)
    val ancestors = this.ancestors(td.impl.parents)
    val body = td.impl.body.flatMap(optBodyItem(_, Some(td)))
    val selfType = this.selfType(td.impl.self)
    val name = td.name.toString
    val companion = findCompaion(name, parentScope)
    val annotations = parseAnnotations(td)((n,as) => STraitOrClassAnnotation(n,as.map(parseExpr)))
    STraitDef(name, tpeArgs, ancestors, body, selfType, companion, annotations)
  }

  def classDef(cd: ClassDef, parentScope: Option[ImplDef]): SClassDef = {
    val ancestors = this.ancestors(cd.impl.parents)
    val constructor = (cd.impl.body.collect {
      case dd: DefDef if dd.name == nme.CONSTRUCTOR => dd
    }) match {
      case Seq(only) => only
      case seq => !!!(s"Class ${cd.name} should have 1 constructor but has ${seq.length} constructors", cd)
    }
    // TODO simplify
    val (args, implicitArgs) = constructor.vparamss match {
      case Seq() =>
        (classArgs(List.empty), classArgs(List.empty))
      case Seq(nonImplConArgs) =>
        (classArgs(nonImplConArgs), classArgs(List.empty))
      case Seq(nonImplConArgs, implConArgs) =>
        (classArgs(nonImplConArgs), classArgs(implConArgs))
      case seq => !!!(s"Constructor of class ${cd.name} has more than 2 parameter lists, not supported")
    }
    val tpeArgs = this.tpeArgs(cd.tparams, constructor.vparamss.lastOption.getOrElse(Nil))
    val body = cd.impl.body.flatMap(optBodyItem(_, Some(cd)))
    val selfType = this.selfType(cd.impl.self)
    val isAbstract = cd.mods.hasAbstractFlag
    val name = cd.name.toString
    val companion = findCompaion(name, parentScope)
    val annotations = parseAnnotations(cd)((n,as) => STraitOrClassAnnotation(n,as.map(parseExpr)))
    SClassDef(cd.name, tpeArgs, args, implicitArgs, ancestors, body, selfType, companion, isAbstract, annotations)
  }

  def objectDef(od: ModuleDef): SObjectDef = {
    val ancestors = this.ancestors(od.impl.parents)
    val body = od.impl.body.flatMap(optBodyItem(_, Some(od)))
    SObjectDef(od.name, ancestors, body)
  }

  def classArgs(vds: List[ValDef]): SClassArgs = SClassArgs(vds.filter(!isEvidenceParam(_)).map(classArg))

  def classArg(vd: ValDef): SClassArg = {
    val tpe = tpeExpr(vd.tpt)
    val default = optExpr(vd.rhs)
    val isOverride = vd.mods.isAnyOverride
    val isVal = vd.mods.isParamAccessor
    val annotations = parseAnnotations(vd)((n,as) => new SArgAnnotation(n, as.map(parseExpr)))
    SClassArg(vd.mods.isImplicit, isOverride, isVal, vd.name, tpe, default, annotations)
  }

  def traitCall(tree: Tree): STraitCall = tree match {
    case ident: Ident =>
      STraitCall(ident.name, List())
    case select: Select =>
      STraitCall(select.name, List())
    case AppliedTypeTree(tpt, args) =>
      STraitCall(tpt.toString, args.map(tpeExpr))
    case tree => ???(tree)
  }

  def optBodyItem(tree: Tree, parentScope: Option[ImplDef]): Option[SBodyItem] = tree match {
    case i: Import =>
      Some(importStat(i))
    case md: DefDef =>
      if (!nme.isConstructorName(md.name))
        md.tpt match {
          case AppliedTypeTree(tpt, _) if tpt.toString == "Elem" || tpt.toString == "Element" =>
            Some(methodDef(md, true))
          case _ =>
            Some(methodDef(md))
        }
      else
        None
    case td: TypeDef =>
      val tpeArgs = this.tpeArgs(td.tparams, Nil)
      val rhs = tpeExpr(td.rhs)
      Some(STpeDef(td.name, tpeArgs, rhs))
    case td: ClassDef if td.mods.isTrait =>
      Some(traitDef(td, parentScope))
    case cd: ClassDef if !cd.mods.isTrait => // isClass doesn't exist
      Some(classDef(cd, parentScope))
    case od: ModuleDef =>
      Some(objectDef(od))
    case vd: ValDef =>
      if (!vd.mods.isParamAccessor) {
        val tpeRes = optTpeExpr(vd.tpt)
        val isImplicit = vd.mods.isImplicit
        val isLazy = vd.mods.isLazy
        Some(SValDef(vd.name, tpeRes, isImplicit, isLazy, parseExpr(vd.rhs)))
      } else
        None
    case EmptyTree =>
      None
    case tree => ???(tree)
  }

  object ExtractAnnotation {
    def unapply(a: Tree): Option[(String, List[Tree])] = a match {
      case Apply(Select(New(Ident(ident)), nme.CONSTRUCTOR), args) => Some((ident, args))
      case _ => None
    }
  }

  def parseAnnotations[A <: SAnnotation](md: MemberDef)(p: (String, List[Tree]) => A): List[A] = {
    val annotations = md.mods.annotations.map {
      case ExtractAnnotation (name, args) => p(name, args)
      case a => !!! (s"Cannot parse annotation $a of MemberDef $md")
    }
    annotations
  }

  class HasAnnotation(annClass: String) {
    def unapply(md: MemberDef): Option[List[Tree]] =
      md.mods.annotations.collectFirst {
        case ExtractAnnotation(name, args) if name == annClass => args
      }
  }

  //  val HasExternalAnnotation = new ExtractAnnotation("External")
  //  val HasConstructorAnnotation = new ExtractAnnotation("Constructor")
  val HasArgListAnnotation = new HasAnnotation("ArgList")
  val OverloadIdAnnotation = new HasAnnotation("OverloadId")

  def methodDef(md: DefDef, isElem: Boolean = false) = {
    val tpeArgs = this.tpeArgs(md.tparams, md.vparamss.lastOption.getOrElse(Nil))
    val args0 = md.vparamss.map(methodArgs)
    val args = if (!args0.isEmpty && args0.last.args.isEmpty) args0.init else args0
    val tpeRes = optTpeExpr(md.tpt)
    val isImplicit = md.mods.isImplicit
    val optOverloadId = md match {
      case OverloadIdAnnotation(List(Literal(Constant(overloadId)))) =>
        Some(overloadId.toString)
      case _ => None
    }
    val annotations = md.mods.annotations.map {
      case ExtractAnnotation(name, args) => SMethodAnnotation(name, args.map(parseExpr))
      case a => !!!(s"Cannot parse annotation $a of the method $md")
    }
    //    val optExternal = md match {
    //      case HasExternalAnnotation(_) => Some(ExternalMethod)
    //      case HasConstructorAnnotation(_) => Some(ExternalConstructor)
    //      case _ => None
    //    }
    val optBody:Option[SExpr] = md.rhs match {
      case Apply(ident:Ident, args) if ident.name.intern() == "sql" =>
        Some(SApply(SLiteral("sql"), List(SLiteral(args(0).asInstanceOf[Literal].value.stringValue))))
      case _ => optExpr(md.rhs)
    }
    val optElem = if (isElem) Some(()) else None
    SMethodDef(md.name, tpeArgs, args, tpeRes, isImplicit, optOverloadId, annotations, optBody, optElem)
  }

  def methodArgs(vds: List[ValDef]): SMethodArgs = vds match {
    case Nil => SMethodArgs(List.empty)
    case vd :: _ =>
      SMethodArgs(vds.filter(!isEvidenceParam(_)).map(methodArg))
  }

  def optTpeExpr(tree: Tree): Option[STpeExpr] = {
    tree match {
      case _ if tree.isEmpty => None
      case _: ExistentialTypeTree => None
      case tree => Some(tpeExpr(tree))
    }
  }

  def tpeExpr(tree: Tree): STpeExpr = tree match {
    case ident: Ident =>
      val name = ident.name.toString
      STpePrimitives.getOrElse(name, STraitCall(name, List()))
    case select: Select =>
      STraitCall(select.name, List())
    case AppliedTypeTree(tpt, args) =>
      val argTpeExprs = args.map(tpeExpr)
      val genericTypeString = tpt.toString
      if (genericTypeString.contains("scala.Tuple"))
        STpeTuple(argTpeExprs)
      else if (genericTypeString.contains("scala.Function")) {
        val domainTpeExpr = argTpeExprs.length match {
          case 2 => argTpeExprs(0)
          case n => STpeTuple(argTpeExprs.init)
        }
        STpeFunc(domainTpeExpr, argTpeExprs.last)
      } else
        STraitCall(tpt.toString, argTpeExprs)
    case Annotated(_, arg) =>
      tpeExpr(arg)
    case TypeBoundsTree(lo, hi) => STpeTypeBounds(tpeExpr(lo), tpeExpr(hi))
    case tree => ???(tree)
  }

  def methodArg(vd: ValDef): SMethodArg = {
    val tpe = tpeExpr(vd.tpt)
    val default = optExpr(vd.rhs)
    val annotations = parseAnnotations(vd)((n,as) => new SArgAnnotation(n, as.map(parseExpr)))
    val isOverride = vd.mods.isAnyOverride
    SMethodArg(vd.mods.isImplicit, isOverride, vd.name, tpe, default, annotations)
  }

  def selfType(vd: ValDef): Option[SSelfTypeDef] = {
    val components = vd.tpt match {
      case t if t.isEmpty =>
        Nil
      case CompoundTypeTree(Template(ancestors, _, _)) =>
        ancestors.map(tpeExpr)
      case t =>
        List(tpeExpr(t))
    }

    if (components.isEmpty)
      None
    else
      Some(SSelfTypeDef(vd.name.toString, components))
  }

  def optExpr(tree: Tree): Option[SExpr] = {
    if (tree.isEmpty)
      None
    else
      Some(parseExpr(tree))
  }

  def parseExpr(tree: Tree): SExpr = tree match {
    case EmptyTree => SEmpty()
    case Literal(Constant(c)) => SConst(c)
    case Ident(TermName(name)) => SIdent(name)
    case q"$name.super[$qual].$field" => SSuper(name, qual, field)
    case q"$expr.$tname" => SSelect(parseExpr(expr), tname)
    case Apply(Select(New(name), termNames.CONSTRUCTOR), args) =>
      SContr(name.toString(), args.map(parseExpr))
    case q"$expr(..$exprs)" => SApply(parseExpr(expr), exprs.map(parseExpr))
    case Block(init, last) => SBlock(init.map(parseExpr), parseExpr(last))
    case q"$mods val $tname: $tpt = $expr" =>
      SValDef(tname, optTpeExpr(tpt), mods.isLazy, mods.isImplicit, parseExpr(expr))
    case q"if ($cond) $th else $el" => SIf(parseExpr(cond), parseExpr(th), parseExpr(el))
    case q"$expr: $tpt" => SAscr(parseExpr(expr), tpeExpr(tpt))
    case q"(..$params) => $expr" => SFunc(params.map(parseExpr), parseExpr(expr))
    case q"$tpname.this" => SThis(tpname)
    case bi => optBodyItem(bi, None) match {
      case Some(item) => item
      case None => print("Error parsing of " + showRaw(bi)); SDefaultExpr("Error parsing")
    }
  }
}
