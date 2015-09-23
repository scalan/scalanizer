package scalan.plugin

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import scala.tools.nsc._
import scala.tools.nsc.plugins.{PluginComponent, Plugin}
import scalan.meta.ScalanAst._
import scalan.meta.{CodegenConfig, ScalanParsers}

class ScalanPluginComponent(val global: Global)
  extends PluginComponent with ScalanParsers with Enricher with HotSpots with Backend {

  type Compiler = global.type
  val compiler: Compiler = global
  import compiler._

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
          externalTypeToWrapper _,
          composeParentWithExt _,
          addModuleAncestors _, addEntityAncestors _,
          updateSelf _,
          repSynonym _,
          addImports _,
          //addDefaultElem _,
          checkEntityCompanion _, checkClassCompanion _,
          cleanUpClassTags _, replaceClassTagByElem _, eliminateClassTagApply _,
          genEntityImpicits _, genClassesImplicits _, genMethodsImplicits _,
          fixEntityCompanionName _
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

  def getStagedAst(module: SEntityModuleDef,
                   cake: Tree, impl: Tree, exts: List[Tree], serial: Tree,
                   hotSpotKernels: Tree, hotSpotManager: Tree): Tree = {
    val implContent = impl match {
      case PackageDef(_, topstats) => topstats.flatMap{ _ match {
        case PackageDef(Ident(TermName("impl")), stats) => stats
      }}
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

  def getExtensions(module: SEntityModuleDef): List[Tree] = {
    genModuleExtensions(module).map(extTrait => genTrait(extTrait)(GenCtx(module, false)))
  }

  def serializeAst(module: SEntityModuleDef): Tree = {
    val str = if (ScalanPluginConfig.saveMetaAst) {
      val bos = new ByteArrayOutputStream()
      val objOut = new ObjectOutputStream(bos)

      /* Erasing of the module: give up Scala Trees */
      val erasedModule = eraseModule(module)

      objOut.writeObject(erasedModule)
      objOut.close()

      javax.xml.bind.DatatypeConverter.printBase64Binary(bos.toByteArray)
    } else ""
    val serialized = global.Literal(Constant(str))

    q"val serializedMetaAst = $serialized"
  }

  def config: CodegenConfig = ScalanPluginConfig.codegenConfig
}

class ScalanPlugin(val global: Global) extends Plugin {
  /** Visible name of the plugin */
  val name: String = "scalan"

  /** The compiler components that will be applied when running this plugin */
  val components: List[PluginComponent] = ScalanPlugin.components(global)

  /** The description is printed with the option: -Xplugin-list */
  val description: String = "Optimization through staging"

  /** Pluging-specific options without -P:scalan:  */
  override def processOptions(options: List[String], error: String => Unit) {
    options foreach {
      case "save" => ScalanPluginConfig.save = true
      case "read" => ScalanPluginConfig.read = true
      case "debug" => ScalanPluginConfig.debug = true
      case option => error("Option not understood: " + option)
    }
  }

  /** A description of the plugin's options */
  override val optionsHelp = Some(
    "  -P:"+ name +":save     Save META boilerplate and virtualized code to a file.\n"+
    "  -P:"+ name +":read     Read META boilerplate and virtualized code from a file.\n"+
    "  -P:"+ name +":debug    Print debug information: final AST and etc.\n"
  )
}

object ScalanPluginState {
  import scala.collection.mutable.Map
  /** Mapping of module name to its extensions that should be generated by the plugin.
    * For example: Segments -> Set(SegmentsDsl, SegmentsDslSeq, SegmentsDslExp) */
  val extMap = Map[String, Set[String]](
    "WPredefs" -> Set("WPredefsDsl", "WPredefsDslSeq", "WPredefsDslExp"),
    "WGenIterables" -> Set("WGenIterablesDsl", "WGenIterablesDslSeq", "WGenIterablesDslExp"),
    "WCanBuildFroms" -> Set("WCanBuildFromsDsl", "WCanBuildFromsDslSeq", "WCanBuildFromsDslExp"),
    "WArrays" -> Set("WArraysDsl", "WArraysDslSeq", "WArraysDslExp"),
    "WArrayOpss" -> Set("WArrayOpssDsl", "WArrayOpssDslSeq", "WArrayOpssDslExp"),
    "WWrappedArrays" -> Set("WWrappedArraysDsl", "WWrappedArraysDslSeq", "WWrappedArraysDslExp"),
    "WNums" -> Set("WNumsDsl", "WNumsDslSeq", "WNumsDslExp"),
    "WDoubleNums" -> Set("WDoubleNumsDsl", "WDoubleNumsDslSeq", "WDoubleNumsDslExp"),
    "WNumMonoids" -> Set("WNumMonoidsDsl", "WNumMonoidsDslSeq", "WNumMonoidsDslExp"),
    "WPlusMonoids" -> Set("WPlusMonoidsDsl", "WPlusMonoidsDslSeq", "WPlusMonoidsDslExp"),
    "WCols" -> Set("WColsDsl", "WColsDslSeq", "WColsDslExp"),
    "WVecs" -> Set("WVecsDsl", "WVecsDslSeq", "WVecsDslExp"),
    "WDenseVecs" -> Set("WDenseVecsDsl", "WDenseVecsDslSeq", "WDenseVecsDslExp"),
    "WMatrs" -> Set("WMatrsDsl", "WMatrsDslSeq", "WMatrsDslExp"),
    "WDenseMatrs" -> Set("WDenseMatrsDsl", "WDenseMatrsDslSeq", "WDenseMatrsDslExp"),
    "WMatrOps" -> Set("WMatrOpsDsl", "WMatrOpsDslSeq", "WMatrOpsDslExp"),
    "WBaseMatrOps" -> Set("WBaseMatrOpsDsl", "WBaseMatrOpsDslSeq", "WBaseMatrOpsDslExp")
  )

  ScalanPluginConfig.codegenConfig.entityFiles.foreach { file =>
    val fileNameParts = file.split('.')
    if (!fileNameParts.isEmpty) {
      val moduleName = fileNameParts.head
      extMap(moduleName) = Set(moduleName + "Dsl", moduleName + "DslSeq", moduleName + "DslExp")
    }
  }

  /** Mapping between modules and another modules used by them. */
  val usageMap = Map[String, List[String]](
    "Nums" -> List("LinearAlgebra"),
    "NumMonoids" -> List("Nums","LinearAlgebra"),
    "Cols" -> List("NumMonoids","LinearAlgebra"),
    "Vecs" -> List("NumMonoids", "Cols", "LinearAlgebra"),
    "Matrs" -> List("NumMonoids", "Cols", "Vecs", "LinearAlgebra"),
    "MatrOps" -> List("Nums", "NumMonoids", "Vecs", "Matrs", "LinearAlgebra"),
    "LinearAlgebra" -> List("NumMonoids", "Cols", "Vecs", "Matrs"),
    "LinearAlgebraOps" -> List("LinearAlgebra")
  )

  /** Mapping of module name to the package where it is defined. */
  val pkgOfModule = Map[String, String](
    "Nums" -> "scalanizer",
    "NumMonoids" -> "scalanizer",
    "Cols" -> "scalanizer.collections",
    "Vecs" -> "scalanizer.linalgebra",
    "Matrs" -> "scalanizer.linalgebra",
    "MatrOps" -> "scalanizer.linalgebra",
    "LinearAlgebra" -> "scalanizer.linalgebra",
    "LinearAlgebraOps" -> "scalanizer.linalgebra"
  )

  /** Mapping of external type names to their wrappers. */
  val wrappers = Map[String, SEntityModuleDef]()

  val externalTypes = Set[String](
    "Predef",
    "GenIterable",
    "CanBuildFrom",
    "Array", "ArrayOps", "WrappedArray"
  )
}

object ScalanPlugin {
  /** Yields the list of Components to be executed in this plugin */
  def components(global: Global) = {
    val result = scala.collection.mutable.ListBuffer[PluginComponent](
      new WrapFrontend(global)
      ,new WrapEnricher(global)
      ,new WrapBackend(global)
      ,new CheckExtensions(global)
      ,new ScalanPluginComponent(global)
//      ,new Debug(global)
    )

    result.toList
  }
}
