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

  /** Checks that the method has @HotSpot */
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
    case sel @ Select(objSel @ Apply(TypeApply(_, _), _), member) =>
      updateWrapper(objSel.tpe, member, sel.tpe, sel.symbol)
    case sel @ Select(objSel @ Select(_, obj), member) if isWrapper(objSel.tpe.typeSymbol) =>
      updateWrapper(objSel.tpe, member, sel.tpe, sel.symbol)
    case sel @ Select(objSel, member) if isWrapper(objSel.tpe.typeSymbol) =>
      updateWrapper(objSel.tpe, member, sel.tpe, sel.symbol)
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

  def getExtTypeAncestors(externalType: Type): List[STraitCall] = {
    val externalTypeDecl = externalType.typeSymbol.typeSignature
    def convToMetaType(types: List[Type]): List[STraitCall] = {
      types map parseType collect {case t: STraitCall => t}
    }
    val ancestors = externalTypeDecl match {
      case PolyType(_, ClassInfoType(parents,_,_)) => convToMetaType(parents)
      case ClassInfoType(parents,_,_) => convToMetaType(parents)
      case _ => Nil
    }

    ancestors.filterNot(_.name == "AnyRef")
  }

  /** Creates Meta Module for an external type symbol. For example:
    * trait WCols extends Base with TypeWrappers { self: WrappersDsl =>
    *   trait WCol[A] extends TypeWrapper[Col[A], WCol[A]] { self =>
    *     def arr: Array[A]
    *   };
    *   trait WColCompanion extends ExCompanion1[WCol]
    * }
    * where
    *   externalType is "class Col"
    *   one of the members is "def arr: Array[A]"
    * */
  def createWrapper(externalType: Type, members: List[SBodyItem]): SEntityModuleDef = {
    val externalTypeSym = externalType.typeSymbol
    val clazz = externalTypeSym.companionClass
    val className = wrap(clazz.nameString)
    val companionName = comp(className)
    val isCompanion = externalTypeSym.isModuleClass

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
      ancestors = STraitCall(
        "TypeWrapper",
        List(STraitCall(clazz.nameString, typeParams), STraitCall(className, typeParams))
      ) :: getExtTypeAncestors(externalType),
      body =  if (isCompanion) Nil else members,
      selfType = Some(SSelfTypeDef("self", Nil)),
      companion = Some(STraitDef(
        name = companionName,
        tpeArgs = Nil,
        ancestors = List(STraitCall("ExCompanion" + typeParams.length.toString, List(STraitCall(className, Nil)))),
        body = if (isCompanion) members else Nil,
        selfType = None, companion = None
      ))
//      , annotations = if (typeParams.isEmpty) Nil else List(STraitOrClassAnnotation("ContainerType", Nil))
    )
    val imports = List(
      SImportStat("scalan._"),
      SImportStat("scalan.common.Default"),
      SImportStat("impl._"),
      SImportStat(externalTypeSym.fullName)
    )

    SEntityModuleDef(
      packageName = "wrappers",
      imports = imports,
      name = wmod(externalTypeSym.nameString),
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

  /** Adds a method or a value to the wrapper. It checks the external type symbol
    * to determine where to put the method (value) - into class or its companion. */
  def addMember(externalType: Type, member: SMethodDef, module: SEntityModuleDef): SEntityModuleDef = {
    val isCompanion = externalType.typeSymbol.isModuleClass
    def isAlreadyAdded = {
      if (isCompanion) {
        module.entityOps.companion match {
          case Some(companion) => companion.body.contains(member)
          case None => false
        }
      } else module.entityOps.body.contains(member)
    }

    if (isAlreadyAdded) {
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

  /** Create/update Meta AST of the module for the external type. It assembles
    * Meta AST of a method (value) by its Scala's Type. */
  def updateWrapper(objType: Type,
                    methodName: Name, methodType: Type, methodSym: Symbol): Unit = {
    val externalTypeName = objType.typeSymbol.nameString
    val owner = methodSym.owner
    val pre = objType.typeSymbol.typeSignature
    val memberType = methodSym.tpe.asSeenFrom(pre, owner)
    val member = memberType match {
      case method @ (_:NullaryMethodType | _:MethodType) =>
        /* Not polymorphic methods like:
         *   trait Col[A] {
         *     def arr: Array[A]
         *     def apply(i: Int): A
         *   }
         **/
        val (args, res) = uncurryMethodType(method)
        formMethodDef(methodName.toString, Nil, args, res)
      case PolyType(typeArgs, method @ (_:NullaryMethodType | _:MethodType)) =>
        /* Methods that have type parameters like:
         * object Col {
         *   def apply[T: ClassTag](arr: Array[T]): Col[T] = fromArray(arr)
         *   def fromArray[T: ClassTag](arr: Array[T]): Col[T] = new ColOverArray(arr)
         * }
         **/
        val tpeArgs = formMethodTypeArgs(typeArgs)
        val (args, res) = uncurryMethodType(method)

        formMethodDef(methodName.toString, tpeArgs, args, res)
      case TypeRef(_,sym,_) =>
        /* Example: arr.length where
         * arr has type MyArr[Int] and
         * class MyArr[T] {
         *   val length = 0
         * }
         **/
        formMethodDef(methodName.toString, Nil, Nil, formMethodRes(sym.tpe))
      case _ => throw new NotImplementedError(s"memberType = ${showRaw(memberType)}")
    }
    val updatedModule = ScalanPluginState.wrappers.get(externalTypeName) match {
      case None => createWrapper(objType, List(member))
      case Some(module) => addMember(objType, member, module)
    }

    ScalanPluginState.wrappers(externalTypeName) = updatedModule
    createDependencies(memberType)
  }

  def uncurryMethodType(method: Type): (List[SMethodArgs], STpeExpr) = {
    def addArgSection(sections: List[SMethodArgs], args: List[Symbol]): List[SMethodArgs] = {
      val methodArgs = formMethodArgs(args)

      if (methodArgs.isEmpty) sections
      else sections :+ SMethodArgs(methodArgs)
    }
    @tailrec
    def loop(currMethod: Type, currArgs: List[SMethodArgs]): (List[SMethodArgs], STpeExpr) = {
      currMethod match {
        case NullaryMethodType(method) => loop(method, currArgs)
        case MethodType(args, method: MethodType) => loop(method, addArgSection(currArgs, args))
        case MethodType(args, resType) => (addArgSection(currArgs, args), parseType(resType))
        case tref @ (_:AbstractTypeRef | _:NoArgsTypeRef | _:ArgsTypeRef) =>
          (currArgs, parseType(tref))
      }
    }
    loop(method, Nil)
  }

  /** Traversing of the type and adding of wrappers for external types. */
  def createDependencies(memberType: Type): Unit = {
    class DependencyTraverser extends TypeTraverser {
      def traverse(tp: Type): Unit = tp match {
        case TypeRef(pre, sym, args) if isWrapper(sym) =>
          if (!ScalanPluginState.wrappers.contains(sym.nameString)) {
            val module = createWrapper(sym.tpe, Nil)
            ScalanPluginState.wrappers(sym.nameString) = module
          }
        case _ => mapOver(tp)
      }
    }

    new DependencyTraverser().traverse(memberType)
  }

  def config: CodegenConfig = ScalanPluginConfig.codegenConfig
}