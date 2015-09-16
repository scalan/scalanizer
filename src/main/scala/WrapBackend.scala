package scalan.plugin

import scalan.util.FileUtil
import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scalan.meta.ScalanAst._

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
    val seqAncestors = if (ScalanPluginConfig.codegenConfig.isSeqEnabled)
                         cake.seq.ancestors :+ STraitCall(module.name + "DslSeq", Nil)
                       else
                         cake.seq.ancestors
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
