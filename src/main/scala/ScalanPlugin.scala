package scalan.plugin

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import scala.tools.nsc._
import scala.tools.nsc.plugins.{PluginComponent, Plugin}
import scala.reflect.internal.util.BatchSourceFile
import scalan.meta.ScalanAst._
import scalan.meta.{CodegenConfig, ScalanParsers}

class ScalanPluginComponent(val global: Global)
  extends PluginComponent with ScalanParsers with Enricher with Backend {

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
          addModuleAncestors _, addEntityAncestors _,
          updateSelf _,
          repSynonym _,
          addImports _,
          //addDefaultElem _,
          checkEntityCompanion _, checkClassCompanion _,
          genEntityImpicits _, genClassesImplicits _, genMethodsImplicits _
        ))
        val enrichedMetaAst = pipeline(metaAst)

        /** Boilerplate generation */
        val entityGen = new scalan.meta.ScalanCodegen.EntityFileGenerator(
          enrichedMetaAst, ScalanPluginConfig.codegenConfig)
        val implCode = entityGen.getImplFile
        val implCodeFile = new BatchSourceFile("<impl>", implCode)
        val boilerplate = newUnitParser(new CompilationUnit(implCodeFile)).parse()

        /** Generates a duplicate of original Scala AST, wraps types by Rep[] and etc. */
        val virtAst = genScalaAst(enrichedMetaAst, unit.body)

        /** Checking of user's extensions like SegmentDsl, SegmentDslSeq and SegmentDslExp */
        val extensions = getExtensions(metaAst)

        /** Prepare Virtualized AST for passing to run-time. */
        val pickledAst = serializeAst(metaAst)

        /** Staged Ast is package which contains virtualized Tree + boilerplate */
        val stagedAst = getStagedAst(metaAst, virtAst, boilerplate, extensions, pickledAst)

        if (ScalanPluginConfig.save) {
          saveImplCode(unit.source.file.file, showCode(stagedAst))
        }

        if (!ScalanPluginConfig.read) {
          unit.body = combineAst(unit.body, stagedAst)
        }
      } catch {
        case e: Exception => print(s"Error: failed to parse ${unitName} due to " + e.printStackTrace())
      }
    }
  }

  def getStagedAst(module: SEntityModuleDef,
                   cake: Tree, impl: Tree, exts: List[Tree], serial: Tree): Tree = {
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
          List(PackageDef(Ident(TermName("implOf"+module.name)), List(stagedObj)))
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
    def getModuleSelfTypeName(module: SEntityModuleDef): String = module.selfType match {
      case Some(selfType) if !selfType.components.isEmpty => selfType.components.head.name
      case _ => module.name
    }
    val extNames = ScalanPluginState.emap(module.name)
    val parentSuffix = Map("Dsl" -> "Abs", "DslSeq" -> "Seq", "DslExp" -> "Exp")
    val extInfo = extNames.map { extName =>
      val extSuffix = extName.stripPrefix(module.name)
      val selfTypeName = getModuleSelfTypeName(module)
      (extName, module.name + parentSuffix(extSuffix), selfTypeName + extSuffix)
    }

    extInfo.map(info => {
      val (extName, parentName, selfTypeName) = info
      val extTree = TypeName(extName)
      val parentTree = TypeName(parentName)
      val selfType = TypeName(selfTypeName)

      q"trait $extTree extends $parentTree { self: $selfType => }" : Tree
    }).toList
  }

  def serializeAst(module: SEntityModuleDef): Tree = {
    val bos = new ByteArrayOutputStream()
    val objOut = new ObjectOutputStream(bos)

    /* Erasing of the module: give up Scala Trees */
    val erasedModule = eraseModule(module)

    objOut.writeObject(erasedModule)
    objOut.close()

    val str = javax.xml.bind.DatatypeConverter.printBase64Binary(bos.toByteArray)
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
      case "debug"     => ScalanPluginConfig.debug = true
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
  val emap = scala.collection.mutable.Map[String, Set[String]]()

  ScalanPluginConfig.codegenConfig.entityFiles.foreach { file =>
    val fileNameParts = file.split('.')
    if (!fileNameParts.isEmpty) {
      val moduleName = fileNameParts.head
      emap(moduleName) = Set(moduleName + "Dsl", moduleName + "DslSeq", moduleName + "DslExp")
    }
  }
}

object ScalanPlugin {
  /** Yields the list of Components to be executed in this plugin */
  def components(global: Global) = {
    val result = scala.collection.mutable.ListBuffer[PluginComponent](
      new CheckExtensions(global)
      ,new ScalanPluginComponent(global)
      //,new Debug(global)
    )

    result.toList
  }
}
