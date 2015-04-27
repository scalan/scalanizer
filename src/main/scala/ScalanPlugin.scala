package scalan.plugin

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import scala.tools.nsc._
import scala.tools.nsc.plugins.{PluginComponent, Plugin}
import scala.reflect.internal.util.BatchSourceFile
import ScalanAst._

object ScalanPluginConfig {
  var save: Boolean = true
  var read: Boolean = true
  var debug: Boolean = false
  val files = List[String](
    "Segms.scala"
    //, "Knds.scala"
  )
  val baseContextTrait = "ScalanDsl"
  val seqContextTrait = "ScalanSeq"
  val stagedContextTrait = "ScalanExp"
  val extraImports = List(
    "scala.reflect.runtime.universe._",
    "scala.reflect._",
    "scalan.common.Default"
  )
  val entityTypeSynonyms = Map[String, String](
    "RepSegm" -> "Segm"
    ,"RKind" -> "Kind"
  )
}

trait ScalanPluginCake extends ScalanParsers with ScalanUtils
with ScalanCodegen with ScalanAstExtensions
with SqlCompiler with SqlAST with SqlParser
with GenScalaAst

class ScalanPluginComponent(val global: Global) extends PluginComponent
with ScalanPluginCake { self: ScalanPluginCake =>
  import global._
  val compiler: Global = global

  val phaseName: String = "scalan"
  override def description: String = "Code virtualization and specialization"

  val runsAfter = List[String]("scalan-check")
  override val runsRightAfter: Option[String] = Some("scalan-check")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      val unitName = unit.source.file.name

      if (ScalanPluginConfig.files.contains(unitName)) try {
        val metaAst = parse(unit.body)
        /** Transformations of Scalan AST */
        val pipeline = scala.Function.chain(Seq(
          addAncestors _,
          updateSelf _,
          repSynonym _,
          addImports _,
          addDefaultElem _,
          checkEntityCompanion _, checkClassCompanion _
        ))
        val virtAst = pipeline(metaAst)

        /** Boilerplate generation */
        val entityGen = new EntityFileGenerator(virtAst)
        val implCode = entityGen.getImplFile
        val implCodeFile = new BatchSourceFile("<impl>", implCode)
        val implAst = newUnitParser(new CompilationUnit(implCodeFile)).parse()

        /** Generates a duplicate of original Scala AST, wraps types by Rep[] and etc. */
        val virtScalaAst = genScalaAst(virtAst, unit.body)

        /** Checking of user's extensions like SegmentDsl, SegmentDslSeq and SegmentDslExp */
        val extensions = getExtensions(metaAst)

        /** Prepare Virtualized AST for passing to run-time. */
        val serialAst = serializeAst(virtAst)

        /** Staged Ast is package which contains virtualized Tree + boilerplate */
        val stagedAst = getStagedAst(virtScalaAst, implAst, extensions, serialAst)

        if (ScalanPluginConfig.save) {
          saveImplCode(unit.source.file.file, showCode(stagedAst))
        }

        if (!ScalanPluginConfig.read) {
          unit.body = combineAst(unit.body, stagedAst)
        }
      } catch {
        case e: Exception => print(s"Error: failed to parse ${unitName} due to " + e)
      }
    }
  }

  def getStagedAst(cake: Tree, impl: Tree, exts: List[Tree], serial: Tree): Tree = {
    val implContent = impl match {
      case PackageDef(_, topstats) => topstats.flatMap{ _ match {
        case PackageDef(Ident(TermName("impl")), stats) => stats
      }}
    }
    cake match {
      case PackageDef(pkgName, cakeContent) =>
        val body = implContent ++ cakeContent ++ exts ++ List(serial)
        val stagedObj = q"object StagedEvaluation {..$body}"

        PackageDef(pkgName, List(stagedObj))
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
    val extNames = ScalanPluginState.emap(module.name)
    val psuf = Map("Dsl" -> "Abs", "DslSeq" -> "Seq", "DslExp" -> "Exp")
    val extsWithParents = extNames.map(extName =>
      (extName, module.name + psuf(extName.stripPrefix(module.name)))
    )

    extsWithParents.map(pair => {
      val (extName, parentName) = pair
      val extTree = TypeName(extName)
      val parentTree = TypeName(parentName)

      q"trait $extTree extends $parentTree" : Tree
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
    "  -P:"+ name +":save     Save META boilerplate and staged version to source files.\n"+
    "  -P:"+ name +":read     Read META boilerplate and staged version from source files.\n"+
    "  -P:"+ name +":debug    Print debug information: final AST and etc.\n"
  )
}

object ScalanPluginState {
  var emap = scala.collection.mutable.Map(
    "Segms" -> Set("SegmsDsl", "SegmsDslSeq", "SegmsDslExp")
    , "Knds" -> Set("KndsDsl", "KndsDslSeq", "KndsDslExp")
  )
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
