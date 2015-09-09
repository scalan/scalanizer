package scalan.plugin

import scala.annotation.tailrec
import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scalan.meta.ScalanAst._
import scalan.meta.{CodegenConfig, ScalanParsers}

/** The component builds wrappers. */
class WrapFrontend(val global: Global) extends PluginComponent with Common with ScalanParsers {

  type Compiler = global.type
  val compiler: Compiler = global
  import compiler._

  val phaseName: String = "scalan-wrap-frontend"
  override def description: String = "Building wrappers for external types"

  val runsAfter = List[String]("typer")
  override val runsRightAfter: Option[String] = Some("typer")

  /** The phase creates wrappers for the type that are out of virtualization scope. */
  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      val unitName = unit.source.file.name

      if (ScalanPluginConfig.codegenConfig.entityFiles.contains(unitName)) {
        /* Collect all methods with the HotSpot annotation. */
        val hotSpotFilter = new FilterTreeTraverser(isHotSpotTree)
        hotSpotFilter.traverse(unit.body)
        /* Traversing through the hot spots and building of type wrappers. */
        hotSpotFilter.hits foreach { hotSpot =>
          new ForeachTreeTraverser(catchWrapperUsage).traverse(hotSpot)
        }
      }
    }
  }

  def isHotSpotTree(tree: Tree): Boolean = tree match {
    case method: DefDef =>
      val isHotSpot = method.symbol.annotations exists { annotation =>
        annotation.symbol.nameString == "HotSpot"
      }
      isHotSpot
    case _ => false
  }

  /** For each method call, create type wrapper if the external type should be wrapped. */
  def catchWrapperUsage(tree: Tree): Unit = tree match {
    case sel @ Select(objSel @ Select(_, obj), member) if isWrapper(objSel.tpe.typeSymbol) =>
      updateWrapper(objSel.tpe.typeSymbol, member, sel.tpe, sel.symbol.originalInfo)
    case sel @ Select(objSel, member) if isWrapper(objSel.tpe.typeSymbol) =>
      updateWrapper(objSel.tpe.typeSymbol, member, sel.tpe, sel.symbol.originalInfo)
    case _ => ()
  }

  def isWrapper(sym: Symbol): Boolean = {
    ScalanPluginConfig.externalTypes.contains(sym.nameString)
  }

  /** Form the list of method arguments in terms of Meta AST by using symbols from Scala AST. */
  def formMethodArgs(args: List[Symbol]): List[SMethodArg] = {
    args.map{arg =>
      val tpe = parseType(arg.tpe)
      val isElemOrCont = tpe match {
        case STraitCall("Elem", _) => true
        case STraitCall("Cont", _) => true
        case _ => false
      }
      SMethodArg(
        impFlag = false, overFlag = false,
        name = arg.nameString,
        tpe = tpe,
        default = None, annotations = Nil, isElemOrCont = isElemOrCont
      )
    }
  }

  def formMethodTypeArgs(targs: List[Symbol]): List[STpeArg] = {
    targs.map{targ =>
      STpeArg(
        name = targ.nameString,
        bound = None, contextBound = Nil, tparams = Nil
      )
    }
  }
  def formMethodRes(res: Type): STpeExpr = parseType(res)

  def formMethodDef(name: String,
                    tpeArgs: List[STpeArg],
                    argSections: List[SMethodArgs],
                    tpeRes: STpeExpr): SMethodDef = {
    SMethodDef(
      name = name,
      tpeArgs = tpeArgs,
      argSections = argSections,
      tpeRes = Some(tpeRes),
      isImplicit = false, isOverride = false,
      overloadId = None,
      annotations = List(SMethodAnnotation(annotationClass = "External", args = Nil)),
      body = None,
      isElemOrCont = false
    )
  }

  /** Create Meta Module for a wrapper. */
  def createWrapper(externalType: Symbol, member: SMethodDef): SEntityModuleDef = {
    val clazz = externalType.companionClass
    val className = wrap(clazz.nameString)
    val companionName = comp(className)
    val isCompanion = externalType.isModuleClass

    val tpeArgs = clazz.typeParams.map{ param =>
      STpeArg(
        name = param.nameString,
        bound = None, contextBound = Nil, tparams = Nil
      )
    }
    val typeParams = clazz.typeParams.map{ param =>
      STraitCall(name = param.nameString, tpeSExprs = Nil)
    }
    val entity = STraitDef(
      name = className,
      tpeArgs = tpeArgs,
      ancestors = List(STraitCall(
        "TypeWrapper",
        List(STraitCall(clazz.nameString, typeParams), STraitCall(className, typeParams))
      )),
      body =  if (isCompanion) Nil else List[SBodyItem](member),
      selfType = Some(SSelfTypeDef("self", Nil)),
      companion = Some(STraitDef(
        name = companionName,
        tpeArgs = Nil,
        ancestors = List(STraitCall("ExCompanion" + typeParams.length.toString, List(STraitCall(className, Nil)))),
        body = if (isCompanion) List[SBodyItem](member) else Nil,
        selfType = None, companion = None
      ))
//      , annotations = if (typeParams.isEmpty) Nil else List(STraitOrClassAnnotation("ContainerType", Nil))
    )
    val imports = List(
      SImportStat("scalan._"),
      SImportStat("scalan.common.Default"),
      SImportStat("impl._"),
      SImportStat(externalType.fullName)
    )

    SEntityModuleDef(
      packageName = "wrappers",
      imports = imports,
      name = wmod(externalType.nameString),
      entityRepSynonym = None,
      entityOps = entity, entities = List(entity),
      concreteSClasses = Nil, methods = Nil,
      selfType = Some(SSelfTypeDef(
        name = "self",
        components = List(STraitCall("Wrappers", Nil))
      )),
      body = Nil, seqDslImpl = None,
      ancestors = List(STraitCall("TypeWrappers", Nil))
    )
  }

  /** Add method or value to a wrapper. */
  def addMember(externalType: Symbol, member: SMethodDef, module: SEntityModuleDef): SEntityModuleDef = {
    val isCompanion = externalType.isModuleClass

    if (module.entityOps.body.contains(member)) {
      module
    } else {
      val updatedEntity = if (isCompanion) {
        val updatedCompanion = module.entityOps.companion match {
          case Some(companion: STraitDef) =>
            val updatedBody = member :: companion.body
            Some(companion.copy(body = updatedBody))
          case _ => throw new IllegalArgumentException(module.entityOps.companion.toString)
        }
        module.entityOps.copy(companion = updatedCompanion)
      } else {
        val updatedBody = member :: module.entityOps.body
        module.entityOps.copy(body = updatedBody)
      }
      module.copy(entityOps = updatedEntity, entities = List(updatedEntity))
    }
  }

  /** Create/update Meta AST of the module for the external type. */
  def updateWrapper(externalType: Symbol, memberName: Name,
                    actualMemberType: Type, originalMemberType: Type): Unit = {
    val memberType = originalMemberType
    val member = memberType match {
      case method @ (_:NullaryMethodType | _:MethodType) =>
        val (args, res) = uncurryMethodType(method)
        formMethodDef(memberName.toString, Nil, args, res)
      case PolyType(typeArgs, method @ (_:NullaryMethodType | _:MethodType)) =>
        val tpeArgs = formMethodTypeArgs(typeArgs)
        val (args, res) = uncurryMethodType(method)

        formMethodDef(memberName.toString, tpeArgs, args, res)
      case TypeRef(_,sym,_) => formMethodDef(memberName.toString, Nil, Nil, formMethodRes(sym.tpe))
      case _ => throw new NotImplementedError(s"memberType = ${showRaw(memberType)}")
    }
    val updatedModule = ScalanPluginState.wrappers.get(externalType.nameString) match {
      case None => createWrapper(externalType, member)
      case Some(module) => addMember(externalType, member, module)

    }
    ScalanPluginState.wrappers(externalType.nameString) = updatedModule
  }

  def uncurryMethodType(method: Type): (List[SMethodArgs], STpeExpr) = {
    def addArgSection(sections: List[SMethodArgs], args: List[Symbol]): List[SMethodArgs] = {
      val methodArgs = formMethodArgs(args)

      if (methodArgs.isEmpty) sections
      else SMethodArgs(methodArgs) :: sections
    }
    @tailrec
    def loop(currMethod: Type, currArgs: List[SMethodArgs]): (List[SMethodArgs], STpeExpr) = {
      currMethod match {
        case NullaryMethodType(method) => loop(method, currArgs)
        case MethodType(args, method: MethodType) => loop(method, addArgSection(currArgs, args))
        case MethodType(args, resType) => (addArgSection(currArgs, args), parseType(resType))
        case tref: AbstractTypeRef => (currArgs, parseType(tref.typeSymbol.tpe))
      }
    }
    loop(method, Nil)
  }

  def config: CodegenConfig = ScalanPluginConfig.codegenConfig
}