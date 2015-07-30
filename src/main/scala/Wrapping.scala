package scalan.plugin

import java.io.File
import scalan.util.FileUtil
import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scalan.meta.ScalanAst._
import scalan.meta.{CodegenConfig, ScalanParsers}

/** The component builds wrappers. */
class WrapFrontend(val global: Global) extends PluginComponent with ScalanParsers {

  type Compiler = global.type
  val compiler: Compiler = global
  import compiler._

  val phaseName: String = "scalan-wrap-frontend"
  override def description: String = "Building wrappers for external types"

  val runsAfter = List[String]("typer")
  override val runsRightAfter: Option[String] = Some("typer")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      val unitName = unit.source.file.name

      if (ScalanPluginConfig.codegenConfig.entityFiles.contains(unitName)) {
        newTraverser().traverse(unit.body)
      }
    }
  }

  def newTraverser(): Traverser = new ForeachTreeTraverser(catchWrapperUsage)

  def catchWrapperUsage(tree: Tree): Unit = tree match {
    case sel @ Select(objSel @ Select(_, obj), member) if isWrapper(objSel.tpe.typeSymbol) =>
      updateWrapper(objSel.tpe.typeSymbol, member, sel.tpe, sel.symbol.originalInfo)
    case _ => ()
  }

  def isWrapper(sym: Symbol): Boolean = {
    ScalanPluginConfig.externalTypes.contains(sym.nameString)
  }

  def updateWrapper(externalType: Symbol, memberName: Name,
                    actualMemberType: Type, originalMemberType: Type): Unit = {
    def formMethodDef(name: String, args: List[Symbol], res: Type): SMethodDef = {
      val methodArgs = args.map{arg =>
        SMethodArg(
          impFlag = false, overFlag = false,
          name = arg.nameString,
          tpe = parseType(arg.tpe),
          default = None, annotations = Nil, isElemOrCont = false
        )
      }
      SMethodDef(
        name = name,
        tpeArgs = Nil,
        argSections = List(SMethodArgs(methodArgs)),
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
      case NullaryMethodType(resultType) => formMethodDef(memberName.toString, Nil, resultType)
      case MethodType(args, resultType) => formMethodDef(memberName.toString, args, resultType)
      case TypeRef(_,sym,_) => formMethodDef(memberName.toString, Nil, sym.tpe)
      case _ => throw new NotImplementedError(s"memberType = ${showRaw(memberType)}")
    }
    val updatedModule = ScalanPluginState.wrappers.get(externalType.nameString) match {
      case None =>
        val wrapperName = externalType.nameString + "Wrapper"
        val tpeArgs = externalType.typeParams.map{ param =>
          STpeArg(
            name = param.nameString,
            bound = None, contextBound = Nil, tparams = Nil
          )
        }
        val typeParams = externalType.typeParams.map{ param =>
          STraitCall(name = param.nameString, tpeSExprs = Nil)
        }
        val entity = STraitDef(
          name = wrapperName,
          tpeArgs = tpeArgs,
          ancestors = List(STraitCall(
            "TypeWrapper",
            List(STraitCall(externalType.nameString, typeParams), STraitCall(wrapperName, typeParams))
          )),
          body =  List[SBodyItem](member),
          selfType = Some(SSelfTypeDef("self", Nil)),
          companion = None, annotations = Nil
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
          name = wrapperName + "s",
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
          val updatedBody = member :: module.entityOps.body
          val updatedEntity = module.entityOps.copy(body = updatedBody)
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
          genEntityImpicits _, genMethodsImplicits _,
          defaultMethod _
        ))
        val enrichedModule = pipeline(module)
        //print(enrichedWrapper)
        enrichedModule
      }
    }

    def apply(unit: CompilationUnit): Unit = ()
  }

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
      annotations = Nil, body = Some(SIdent("$qmark$qmark$qmark")),
      isElemOrCont = false
    )
    module.copy(methods = defaultOfWrapper :: module.methods)
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

  def newPhase(prev: Phase) = new StdPhase(prev) {
    override def run(): Unit = {
      ScalanPluginState.wrappers foreach { moduleNameAndAst =>
        val (_, module) = moduleNameAndAst
        implicit val genCtx = GenCtx(module = module, toRep = true)
        /** Form source code of the wrapper and store it. */
        val scalaAst = genModule(module)
        val imports = module.imports.map(genImport(_))
        val selfType = Some(SSelfTypeDef("self", List(STraitCall("Wrappers", Nil))))
        val extensions = genExtensions(module.name, selfType, Nil).map(
          extTrait => genTrait(extTrait)(GenCtx(module, false))
        )
        val pkgStats = imports ++ (scalaAst :: extensions)
        val wrappersPackage = PackageDef(Ident(TermName("wrappers")), pkgStats)
        saveWrappersCode(module.name, showCode(wrappersPackage))

        /** Invoking of Scalan META to produce boilerplate code for the wrapper. */
        val boilerplate = genWrapperBoilerplate(module)
        saveWrapperBoilerplate(module.name, boilerplate)
      }
    }

    def apply(unit: CompilationUnit): Unit = ()
  }

  def genWrapperBoilerplate(module: SEntityModuleDef): String = {
    val gen = new scalan.meta.ScalanCodegen.EntityFileGenerator(
      module, ScalanPluginConfig.codegenConfig)
    val implCode = gen.getImplFile

    implCode
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
}
