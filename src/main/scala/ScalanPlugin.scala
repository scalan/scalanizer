package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.{PluginComponent, Plugin}
import scala.reflect.internal.util.BatchSourceFile

object ScalanConfig {
  var saveMeta: Boolean = false
  var readMeta: Boolean = false
  var debug: Boolean = false
  val files = List[String]("Segms.scala")
  val baseContextTrait = "ScalanDsl"
  val seqContextTrait = "ScalanSeq"
  val stagedContextTrait = "ScalanExp"
  val extraImports = List(
    "scala.reflect.runtime.universe._",
    "scalan.common.Default",
    "scalan.paradise._"
  )
  val entityTypeSynonyms = Map[String, String](
    "RepSegment" -> "Segment",
    "RepSegm" -> "Segm"
  )
  var emap = scala.collection.mutable.Map(
    "Segms" -> Set("SegmsDsl", "SegmsDslSeq", "SegmsDslExp")
  )
}

trait ScalanPluginCake extends ScalanParsers with ScalanUtils
with ScalanCodegen with ScalanAst with ScalanAstExtensions
with SqlCompiler with SqlAST with SqlParser
with CakeSlice

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

      if (ScalanConfig.files.contains(unitName)) try {
        val ast = parse(unit.body)
        /** Transformations of Scalan AST */
        val pipeline = scala.Function.chain(Seq(
          addAncestors _,
          updateSelf _,
          repSynonym _,
          addImports _,
          addDefaultElem _,
          checkEntityCompanion _, checkClassCompanion _
        ))
        val newAst = pipeline(ast)

        /** Boilerplate generation */
        val entityGen = new EntityFileGenerator(newAst)
        val implCode = entityGen.getImplFile

        if (ScalanConfig.saveMeta)
          saveImplCode(unit.source.file.file, implCode)

        val implCodeFile = new BatchSourceFile("<impl>", implCode)
        val implAst = newUnitParser(new CompilationUnit(implCodeFile)).parse()
        /** Creates a duplicate of original Scala AST, wraps types by Rep[] and etc. */
        val cakeSlice = cookCakeSlice(unit.body)

        /** Checking of user's extensions like SegmentDsl, SegmentDslSeq and SegmentDslExp */
        val extensions = getExtensions(ast)

        unit.body = combineAst(unit.body, cakeSlice, implAst, extensions)
        //saveImplCode(unit.source.file.file, showCode(unit.body))

        unit.body
      } catch {
        case e: Exception => print(s"Error: failed to parse ${unitName} due to " + e)
      }
    }
  }

  def combineAst(orig: Tree, cake: Tree, impl: Tree, exts: List[Tree]): Tree = {
    val implContent = impl match {
      case PackageDef(_, topstats) => topstats.flatMap{ _ match {
        case PackageDef(Ident(TermName("impl")), stats) => stats
      }}
    }
    val cakeContent = cake match {
      case PackageDef(_, topstats) => topstats
    }
    val body = implContent ++ cakeContent ++ exts
    val stagedObj = q"object StagedEvaluation {..$body}"
    val newTree = orig match {
      case PackageDef(pkgname, stats: List[Tree]) =>
        PackageDef(pkgname, stats ++ List(stagedObj))
      case _ => orig
    }

    newTree
  }

  def getExtensions(module: SEntityModuleDef): List[Tree] = {
    val extNames = ScalanConfig.emap(module.name)
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
      case "save-meta" => ScalanConfig.saveMeta = true
      case "read-meta" => ScalanConfig.readMeta = true
      case "debug"     => ScalanConfig.debug = true
      case option => error("Option not understood: " + option)
    }
  }

  /** A description of the plugin's options */
  override val optionsHelp = Some(
    "  -P:"+ name +":save-meta     Save META boilerplate to source files.\n"+
    "  -P:"+ name +":read-meta     Read META boilerplate from source files.\n"+
    "  -P:"+ name +":debug         Print debug information: final AST and etc.\n"
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
