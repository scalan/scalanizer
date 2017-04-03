package scalan.plugin

import scala.annotation.tailrec
import scala.tools.nsc._
import scalan.meta.ScalanAst._

object WrapFrontend {
  val name = "scalanizer-frontend"
}

/** The component builds wrappers. */
class WrapFrontend(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
  import scalanizer._
  import scalanizer.global._

  val phaseName: String = WrapFrontend.name
  override def description: String = "Building wrappers for external types"

  val runsAfter = List("typer")

  /** The phase creates wrappers for the type that are out of virtualization scope. */
  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      val unitName = unit.source.file.name

      if (snConfig.codegenConfig.entityFiles.contains(unitName)) {
//        /* Collect all methods with the HotSpot annotation. */
//        val hotSpotFilter = new FilterTreeTraverser(isHotSpotTree)
//        hotSpotFilter.traverse(unit.body)
//        /* Traversing through the hot spots and building of type wrappers. */
//        hotSpotFilter.hits foreach { hotSpot =>
//          new ForeachTreeTraverser(catchWrapperUsage).traverse(hotSpot)
//        }
        new ForeachTreeTraverser(catchWrapperUsage).traverse(unit.body)
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

  /** Applying of the policy: wrap all types outside of virtualization scope. */
  def isWrapperSym(sym: Symbol): Boolean = {
    !sym.hasPackageFlag && sym.isClass && isWrapper(sym.nameString)
  }
  def isWrapperType(tpe: Type): Boolean = isWrapperSym(tpe.typeSymbol)

  /** For each method call, create type wrapper if the external type should be wrapped. */
  def catchWrapperUsage(tree: Tree): Unit = tree match {
    case sel @ Select(objSel @ Apply(TypeApply(_, _), _), member) if isWrapperType(objSel.tpe) =>
      updateWrapper(objSel.tpe, member, sel.tpe, sel.symbol)
    case sel @ Select(objSel @ Select(_, obj), member) if isWrapperType(objSel.tpe) =>
      updateWrapper(objSel.tpe, member, sel.tpe, sel.symbol)
    case sel @ Select(objSel, member) if isWrapperType(objSel.tpe) =>
      updateWrapper(objSel.tpe, member, sel.tpe, sel.symbol)
    case _ => ()
  }

  /** Form the list of method arguments in terms of Meta AST by using symbols from Scala AST. */
  def formMethodArgs(args: List[Symbol]): List[SMethodArg] = {
    args.map{arg =>
      val tpe = parseType(arg.tpe)
      val isTypeDesc = tpe match {
        case STraitCall("Elem", _) => true
        case STraitCall("Cont", _) => true
        case _ => false
      }
      SMethodArg(
        impFlag = false, overFlag = false,
        name = arg.nameString,
        tpe = tpe,
        default = None, annotations = Nil, isTypeDesc = isTypeDesc
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
      isTypeDesc = false
    )
  }

  /** Gets the list of ancestors of the external type in term of Meta AST. */
  def getExtTypeAncestors(externalType: Type): List[STraitCall] = {
    def convToMetaType(types: List[Type]): List[STraitCall] = {
      types map parseType collect {case t: STraitCall => t}
    }
    val ancestors = convToMetaType(getParents(externalType))

    ancestors.filterNot(a => isIgnoredExternalType(a.name))
  }

  /** Gets names of an external type, its class and its module. */
  def wrapperNames(externalType: Type): (String, String, String) = {
    val externalName = externalType.typeSymbol.nameString
    val className = wrap(externalName)

    (externalName, className, comp(className))
  }

  def mkCompanionAncestors(wClassName: String, kind: Int) =
    List(STraitCall("ExCompanion" + kind.toString, List(STraitCall(wClassName, Nil))))

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
  def createWrapper(externalType: Type, members: List[SBodyItem]): WrapperDescr = {
    val externalTypeSym = externalType.typeSymbol
    val clazz = externalTypeSym.companionClass
    val (externalName, wClassName, companionName) = wrapperNames(externalType)
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
    val baseType = if (isCompanion) STraitCall(externalName + ".type", typeParams)
                   else STraitCall(externalName, typeParams)
    val originalEntityAncestors = getExtTypeAncestors(externalType)
    val entityAncestors = STraitCall("TypeWrapper", List(baseType, STraitCall(wClassName, typeParams))) :: originalEntityAncestors

    val entity = STraitDef(
      name = wClassName,
      tpeArgs = tpeArgs,
      ancestors = entityAncestors,
      body =  if (isCompanion) Nil else members,
      selfType = Some(SSelfTypeDef("self", Nil)),
      companion = Some(STraitDef(
        name = companionName,
        tpeArgs = Nil,
        ancestors = mkCompanionAncestors(wClassName, kind = typeParams.length),
        body = if (isCompanion) members else Nil,
        selfType = None, companion = None
      ))
//      , annotations = if (typeParams.isEmpty) Nil else List(STraitOrClassAnnotation("ContainerType", Nil))
    )
    val imports = List(
      SImportStat("scalan._"),
      SImportStat("impl._"),
      SImportStat(externalTypeSym.fullName)
    )

    val module = SModuleDef(
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
      body = Nil, stdDslImpls = None,
      ancestors = List(STraitCall("TypeWrappers", Nil))
    )
    val ownerChain = externalTypeSym.ownerChain.map(_.nameString)

    //createDependencies(externalType)
    WrapperDescr(module, ownerChain)
  }

  /** Adds a method or a value to the wrapper. It checks the external type symbol
    * to determine where to put the method (value) - into class or its companion. */
  def addMember(externalType: Type, member: SMethodDef, wrapperDescr: WrapperDescr): WrapperDescr = {
    val module = wrapperDescr.module
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
      wrapperDescr
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
      wrapperDescr.copy(
        module = module.copy(entityOps = updatedEntity, entities = List(updatedEntity))
      )
    }
  }

  /** Create/update Meta AST of the module for the external type. It assembles
    * Meta AST of a method (value) by its Scala's Type. */
  def updateWrapper(objType: Type,
                    methodName: Name, methodReturnType: Type, methodSym: Symbol): Unit = {
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
    val updatedWrapper = snState.wrappers.get(externalTypeName) match {
      case None =>  createWrapper(objType, List(member))
      case Some(wrapperDescr) => addMember(objType, member, wrapperDescr)
    }

    snState.wrappers(externalTypeName) = updatedWrapper
    createMemberDependencies(memberType)
  }

  /** Converts curried method type its uncurried Meta AST representation. */
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

  /** For the given type, find all dependencies and wrap them. */
  def createDependencies(objType: Type): Unit = {
    val parentDecls = objType.typeSymbol.typeSignature match {
      case PolyType(_, ClassInfoType(parents,_,_)) => parents
      case ClassInfoType(parents,_,_) => parents
      case _ => Nil
    }

    parentDecls foreach {parent =>
      val name = parent.typeSymbol.nameString
      if (!isIgnoredExternalType(name) && !snState.wrappers.keySet.contains(name)) {
        snState.wrappers(name) = createWrapper(parent, Nil)
      }
    }
  }

  /** Traversing of the type and adding of wrappers for external types. */
  def createMemberDependencies(memberType: Type): Unit = {
    class DependencyTraverser extends TypeTraverser {
      def traverse(tp: Type): Unit = tp match {
        case TypeRef(pre, sym, args) if isWrapperSym(sym) =>
          if (!snState.wrappers.contains(sym.nameString)) {
            val module = createWrapper(sym.tpe, Nil)
            snState.wrappers(sym.nameString) = module
          }
        case _ => mapOver(tp)
      }
    }

    new DependencyTraverser().traverse(memberType)
  }
}