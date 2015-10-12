package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scalan.meta.ScalanAst.SEntityModuleDef
import scalan.meta.{ScalanCodegen, CodegenConfig, ScalanParsers}

class ScalanPluginComponent(val global: Global)
  extends PluginComponent with ScalanParsers with Enricher with HotSpots with Backend {

  import global._

  val phaseName: String = "scalan"
  override def description: String = "Code virtualization and specialization"

  val runsAfter = List[String]("scalan-check")
  override val runsRightAfter: Option[String] = Some("scalan-check")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      val unitName = unit.source.file.name
      if (ScalanPluginConfig.codegenConfig.entityFiles.contains(unitName)) try {
        val metaAst = parse(unitName, unit.body)
        /** Transformations of Scalan AST */
        val pipeline = scala.Function.chain(Seq(
          fixExistentialType _,
          externalTypeToWrapper _,
          composeParentWithExt _,
          addModuleAncestors _, addEntityAncestors _,
          updateSelf _,
          repSynonym _,
          addImports _,
          checkEntityCompanion _, checkClassCompanion _,
          cleanUpClassTags _, replaceClassTagByElem _, eliminateClassTagApply _,
          genEntityImpicits _, genClassesImplicits _, genMethodsImplicits _,
          fixEntityCompanionName _,
          fixEvidences _
        ))
        val enrichedMetaAst = pipeline(metaAst)

        /** Invoking of Scalan META to produce boilerplate code */
        val boilerplate = genBoilerplate(enrichedMetaAst)

        /** Generates a duplicate of original Scala AST, wraps types by Rep[] and etc. */
        val virtAst = genScalaAst(enrichedMetaAst, unit.body)

        /** Checking of user's extensions like SegmentDsl, SegmentDslSeq and SegmentDslExp */
        val extensions = getExtensions(metaAst)

        /** Prepare Virtualized AST for passing to run-time. */
        val pickledAst = serializeAst(metaAst)

        /** Accelerated original Scala AST by replacing of hot spots by optimized kernels. */
        val accelAst = transformHotSpots(metaAst, unit)

        /** Staged Ast is package which contains virtualized Tree + boilerplate */
        val stagedAst = getStagedAst(metaAst, virtAst, boilerplate, extensions, pickledAst,
          getHotSpotKernels(metaAst), getHotSpotManager(metaAst))

        if (ScalanPluginConfig.save) {
          saveImplCode(unit.source.file.file, showCode(stagedAst))
        }

        unit.body = accelAst
        /** Discards the generated code and load it from FS. */
        if (!ScalanPluginConfig.read) {
          unit.body = combineAst(unit.body, stagedAst)
        }

        if (ScalanPluginConfig.debug)
          saveDebugCode(unitName, showCode(unit.body))
      } catch {
        case e: Exception => print(s"Error: failed to parse ${unitName} due to " + e.printStackTrace())
      }
    }
  }

  /** Puts all staff in one place. Here is an example for the Matrs module:
    *  package implOfMatrs {
    *    object StagedEvaluation {
    *      // Boilerplate (implContent)
    *      trait MatrsAbs extends Matrs with ScalanDsl {...}
    *      trait MatrsExp extends MatrsDsl with ScalanExp
    *      // Virtualized code (cakeContent)
    *      trait Matrs extends Base {...}
    *      // Auto-generated extensions
    *      trait MatrsDsl extends MatrsAbs {...}
    *      trait MatrsDslExp extends MatrsExp {...}
    *      // Meta AST of the entity converted to string
    *      val serializedMetaAst = "..."
    *    }
    *    // Virtualized hot spots of the module and related staff
    *    object HotSpotKernels {...}
    *    object HotSpotManager {...}
    *  }
    * */
  def getStagedAst(module: SEntityModuleDef,
                   cake: Tree, impl: Tree, exts: List[Tree], serial: Tree,
                   hotSpotKernels: Tree, hotSpotManager: Tree): Tree = {
    val implContent = impl match {
      case PackageDef(_, topstats) => topstats.flatMap {
        case PackageDef(Ident(TermName("impl")), stats) => stats
      }
    }
    cake match {
      case PackageDef(pkgName, cakeContent) =>
        val body = implContent ++ cakeContent ++ exts ++ List(serial)
        val stagedObj = q"object StagedEvaluation {..$body}"

        PackageDef(pkgName,
          List(PackageDef(Ident(TermName("implOf"+module.name)), List(stagedObj, hotSpotKernels, hotSpotManager)))
        )
    }
  }

  /** Puts original and virtualized entity code together. For example:
    * package scalanizer.linalgebra {
    *   trait Matrs {self: LinearAlgebra => }
    * }
    * and the result of the getStagedAst method are combined into:
    * package scalanizer.linalgebra {
    *   trait Matrs {self: LinearAlgebra => ...}
    *   package implOfMatrs {...}
    * }
    * */
  def combineAst(orig: Tree, staged: Tree): Tree = {
    val stagedStats = staged match {
      case PackageDef(_, stats: List[Tree]) => stats
    }
    val newTree = orig match {
      case PackageDef(pkgname, stats: List[Tree]) =>
        PackageDef(pkgname, stats ++ stagedStats)
      case _ => orig
    }

    newTree
  }

  /** Generates extensions like:
    *   trait MatrsDsl extends MatrsAbs { self: LinearAlgebraDsl => };
    *   trait MatrsDslExp extends MatrsExp { self: LinearAlgebraDslExp => };
    * for the module (Matrs).
    */
  def getExtensions(module: SEntityModuleDef): List[Tree] = {
    genModuleExtensions(module).map(extTrait => genTrait(extTrait)(GenCtx(module, false)))
  }

  /** Converts Meta AST of a module to base64 string, assings the string to a variable and
    * returns Scala Tree of the variable. */
  def serializeAst(module: SEntityModuleDef): Tree = {
    val str = if (ScalanPluginConfig.saveMetaAst) {
      val erasedModule = eraseModule(module)
      ScalanCodegen.serialize(erasedModule)
    } else ""
    val serialized = Literal(Constant(str))

    q"val serializedMetaAst = $serialized"
  }

  def config: CodegenConfig = ScalanPluginConfig.codegenConfig
}
