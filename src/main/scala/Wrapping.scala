package scalan.plugin

import java.io.File
import scalan.util.FileUtil
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

  /** Create/update Meta AST of the module for the external type. */
  def updateWrapper(externalType: Symbol, memberName: Name,
                    actualMemberType: Type, originalMemberType: Type): Unit = {
    def formMethodDef(name: String, targs: List[Symbol], args: List[Symbol], res: Type): SMethodDef = {
      val methodArgs = args.map{arg =>
        SMethodArg(
          impFlag = false, overFlag = false,
          name = arg.nameString,
          tpe = parseType(arg.tpe),
          default = None, annotations = Nil, isElemOrCont = false
        )
      }
      val argSections = if (methodArgs.isEmpty) Nil else List(SMethodArgs(methodArgs))
      val tpeArgs = targs.map{targ =>
        STpeArg(
          name = targ.nameString,
          bound = None, contextBound = Nil, tparams = Nil
        )
      }

      SMethodDef(
        name = name,
        tpeArgs = tpeArgs,
        argSections = argSections,
        tpeRes = Some(parseType(res)),
        isImplicit = false, isOverride = false,
        overloadId = None,
        annotations = List(SMethodAnnotation(annotationClass = "External", args = Nil)),
        body = None,
        isElemOrCont = false
      )
    }

    val memberType = originalMemberType
    val member = memberType match {
      case NullaryMethodType(resultType) => formMethodDef(memberName.toString, Nil, Nil, resultType)
      case MethodType(args, resultType) => formMethodDef(memberName.toString, Nil, args, resultType)
      case PolyType(typeArgs, MethodType(args, MethodType(_, resultType))) =>
        formMethodDef(memberName.toString, typeArgs, args, resultType)
      case TypeRef(_,sym,_) => formMethodDef(memberName.toString, Nil, Nil, sym.tpe)
      case _ => throw new NotImplementedError(s"memberType = ${showRaw(memberType)}")
    }
    val isCompanion = externalType.isModuleClass
    val updatedModule = ScalanPluginState.wrappers.get(externalType.nameString) match {
      case None =>
        val clazz = externalType.companionClass
        val className = wrap(clazz.nameString)
        val companionName = comp(className)
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
            tpeArgs = Nil, ancestors = Nil,
            body = if (isCompanion) List[SBodyItem](member) else Nil,
            selfType = None, companion = None
          ))
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
      case Some(module) =>
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
    ScalanPluginState.wrappers(externalType.nameString) = updatedModule
  }

  def config: CodegenConfig = ScalanPluginConfig.codegenConfig
}

/** Virtualization of type wrappers. */
class WrapEnricher(val global: Global) extends PluginComponent with Enricher {

  type Compiler = global.type
  val compiler: Compiler = global

  import compiler._

  val phaseName: String = "scalan-wrap-enricher"

  override def description: String = "Virtualization of type wrappers."

  val runsAfter = List[String]("scalan-wrap-frontend")
  override val runsRightAfter: Option[String] = Some("scalan-wrap-frontend")

  /** The phase prepares a wrapper for virtualization. */
  def newPhase(prev: Phase) = new StdPhase(prev) {
    override def run(): Unit = {
      ScalanPluginState.wrappers transform { (name, module) =>
        /** Transformations of Wrappers by adding of Elem, Cont and other things. */
        val pipeline = scala.Function.chain(Seq(
          addWrappedValue _,
          addModuleAncestors _,
          updateSelf _,
          repSynonym _,
          checkEntityCompanion _,
          genEntityImpicits _,
          genMethodsImplicits _,
          defaultMethod _,
          defaultWrapperImpl _,
          filterConstructor _ // remove methods with name <init>
        ))
        val enrichedModule = pipeline(module)

        enrichedModule
      }
    }

    def apply(unit: CompilationUnit): Unit = ()
  }

  /** Adding of a method which return original external type. For example:
    * def wrappedValueOfBaseType: Rep[Array[T]]; */
  def addWrappedValue(module: SEntityModuleDef): SEntityModuleDef = {
    val resType = module.entityOps.ancestors.collect {
      case STraitCall("TypeWrapper", List(importedType, _)) => importedType
    }.headOption
    val wrappedValueOfBaseType = SMethodDef(
      name = "wrappedValueOfBaseType",
      tpeArgs = Nil, argSections = Nil,
      tpeRes = resType,
      isImplicit = false, isOverride = false, overloadId = None,
      annotations = Nil, body = None, isElemOrCont = false
    )
    val updatedEntity = module.entityOps.copy(
      body = wrappedValueOfBaseType :: module.entityOps.body
    )

    module.copy(entityOps = updatedEntity, entities = List(updatedEntity))
  }

  /** Adding of a method which returns default value of external type.
    * For example: def DefaultOfArray[T]: Default[Array[T]] = ???. */
  def defaultMethod(module: SEntityModuleDef): SEntityModuleDef = {
    val extType = module.entityOps.ancestors.collect {
      case STraitCall("TypeWrapper", List(importedType, _)) => importedType
    }
    val defaultOfWrapper = SMethodDef(
      name = "DefaultOf" + extType.head.name,
      tpeArgs = module.entityOps.tpeArgs,
      argSections = Nil,
      tpeRes = Some(STraitCall("Default", extType)),
      isImplicit = false, isOverride = false, overloadId = None,
      annotations = Nil,
      body = Some(SApply(SSelect(SIdent("Default"),"defaultVal"), Nil, List(List(SConst(null))))),
      isElemOrCont = true // Workaround: disable virtualization of the method
    )
    module.copy(methods = defaultOfWrapper :: module.methods)
  }

  /** Adding of default implementation of the type wrapper. It is required by
    * Scalan Codegen. When the module is stored, the default implementation
    * is filtered. */
  def defaultWrapperImpl(module: SEntityModuleDef): SEntityModuleDef = {
    val wrapperType = module.entityOps.ancestors.collect {
      case STraitCall("TypeWrapper", h :: _) => h
    }.head
    val wrapperImpl = SEntityModuleDef.wrapperImpl(module.entityOps, wrapperType)

    module.copy(concreteSClasses = List(wrapperImpl))
  }

  def filterConstructor(module: SEntityModuleDef): SEntityModuleDef = {
    new MetaAstTransformer {
      override def bodyTransform(body: List[SBodyItem]): List[SBodyItem] = body.filter {
        _ match {
          case m: SMethodDef if m.name == "<init>" => false
          case _ => true
        }
      }
    }.moduleTransform(module)
  }
}


/** Generating of Scala AST for wrappers. */
class WrapBackend(val global: Global) extends PluginComponent with Enricher with Backend {

  type Compiler = global.type
  val compiler: Compiler = global

  import compiler._

  val phaseName: String = "scalan-wrap-backend"

  override def description: String = "Generating of Scala AST for wrappers."

  val runsAfter = List[String]("scalan-wrap-enricher")
  override val runsRightAfter: Option[String] = Some("scalan-wrap-enricher")

  case class WrappersCake(abs: STraitDef, seq: STraitDef, exp: STraitDef)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    override def run(): Unit = {
      var wrappersCake = initWrappersCake
      ScalanPluginState.wrappers foreach { moduleNameAndAst =>
        val (_, module) = moduleNameAndAst

        /** Invoking of Scalan META to produce boilerplate code for the wrapper. */
        val boilerplate = genWrapperBoilerplate(module)
        saveWrapperBoilerplate(module.name, boilerplate)

        /** Form source code of the wrapper and store it. */
        val wrapperWithoutImpl = module.copy(concreteSClasses = Nil)
        val wrappersPackage = genWrapperPackage(wrapperWithoutImpl)
        saveWrappersCode(wrapperWithoutImpl.name, showCode(wrappersPackage))

        wrappersCake = updateWrappersCake(wrappersCake, wrapperWithoutImpl)
      }
      saveWrappersCake(wrappersCake)
    }

    def apply(unit: CompilationUnit): Unit = ()
  }

  def genWrapperBoilerplate(module: SEntityModuleDef): String = {
    val gen = new scalan.meta.ScalanCodegen.EntityFileGenerator(
      module, ScalanPluginConfig.codegenConfig)
    val implCode = gen.getImplFile

    implCode
  }

  def genWrapperPackage(module: SEntityModuleDef): Tree = {
    implicit val genCtx = GenCtx(module = module, toRep = true)
    val scalaAst = genModule(module)
    val imports = module.imports.map(genImport(_))
    val selfType = Some(SSelfTypeDef("self", List(STraitCall("Wrappers", Nil))))
    val extensions = genExtensions(module.name, selfType, Nil).map(
      extTrait => genTrait(extTrait)(GenCtx(module, false))
    )
    val pkgStats = imports ++ (scalaAst :: extensions)
    val wrappersPackage = PackageDef(Ident(TermName("wrappers")), pkgStats)

    wrappersPackage
  }
  def getWrappersHome = ScalanPluginConfig.home + "/src/main/scala/wrappers"
  def saveWrappersCode(fileName: String, wrapperCode: String) = {
    val wrapperFile = FileUtil.file(getWrappersHome, fileName + ".scala")
    wrapperFile.mkdirs()
    FileUtil.write(wrapperFile, wrapperCode)
  }
  def saveWrapperBoilerplate(fileName: String, boilerplate: String): Unit = {
    val boilerplateFile = FileUtil.file(getWrappersHome, "impl", fileName + "Impl.scala")
    boilerplateFile.mkdirs()
    FileUtil.write(boilerplateFile, boilerplate)
  }

  def initWrappersCake: WrappersCake = {
    val abs = STraitDef("WrappersDsl", Nil,
      List(STraitCall("ScalanCommunityDsl", Nil)),
      Nil, None, None)
    val seq = STraitDef("WrappersDslSeq", Nil,
      List(STraitCall("WrappersDsl", Nil), STraitCall("ScalanCommunityDslSeq", Nil)),
      Nil, None, None)
    val exp = STraitDef("WrappersDslExp", Nil,
      List(STraitCall("WrappersDsl", Nil), STraitCall("ScalanCommunityDslExp", Nil)),
      Nil, None, None)

    WrappersCake(abs, seq, exp)
  }

  def updateWrappersCake(cake: WrappersCake, module: SEntityModuleDef): WrappersCake = {
    val absAncestors = cake.abs.ancestors :+ STraitCall(module.name + "Dsl", Nil)
    val seqAncestors = cake.seq.ancestors :+ STraitCall(module.name + "DslSeq", Nil)
    val expAncestors = cake.exp.ancestors :+ STraitCall(module.name + "DslExp", Nil)

    WrappersCake(
      abs = cake.abs.copy(ancestors = absAncestors),
      seq = cake.seq.copy(ancestors = seqAncestors),
      exp = cake.exp.copy(ancestors = expAncestors)
    )
  }

  def saveWrappersCake(cake: WrappersCake): Unit = {
    implicit val genCtx = GenCtx(module = null, toRep = false)
    val absCake = genTrait(cake.abs)
    val seqCake = genTrait(cake.seq)
    val expCake = genTrait(cake.exp)

    val cakePackage =
      q"""
        package wrappers {
          import scalan._

          $absCake

          $seqCake

          $expCake
        }
     """

    saveWrappersCake(showCode(cakePackage))
  }

  def saveWrappersCake(cakes: String): Unit = {
    val wrapperFile = FileUtil.file(getWrappersHome, "Wrappers.scala")
    wrapperFile.mkdirs()
    FileUtil.write(wrapperFile, cakes)
  }
}
