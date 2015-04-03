package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.{PluginComponent, Plugin}
import scala.reflect.internal.util.BatchSourceFile

object ScalanConfig {
  var saveMeta: Boolean = true
  var readMeta: Boolean = false
  var debug: Boolean = false
  val files = List[String]("Segms.scala")
  val baseContextTrait = "ScalanDsl"
  val seqContextTrait = "ScalanSeq"
  val stagedContextTrait = "ScalanExp"
  val extraImports = List(
    "scala.reflect.runtime.universe._",
    "scalan.common.Default")
  val entityTypeSynonyms = Map[String, String](
    "RepSegment" -> "Segment",
    "RepSegm" -> "Segm"
  )
}

trait ScalanPluginCake extends ScalanParsers with ScalanUtils
with ScalanCodegen with ScalanAst with ScalanAstExtensions
with SqlCompiler with SqlAST with SqlParser

class ScalanPluginComponent(val global: Global) extends PluginComponent
with ScalanPluginCake { self: ScalanPluginCake =>
  import global._
  val compiler: Global = global

  val phaseName: String = "scalan"
  override def description: String = "Code virtualization and specialization"

  val runsAfter = List[String]("parser")
  override val runsRightAfter: Option[String] = Some("parser")

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

        unit.body = combineAst(unit.body, implAst)
      } catch {
        case e: Exception => print(s"Error: failed to parse ${unitName} due to " + e)
      }
    }
  }

  def combineAst(orig: Tree, impl: Tree): Tree = {
    val dup = orig.duplicate
    val body = List(dup, impl)
    val stagedObj = q"object StagedEvaluation { ..$body }"
/*
    orig match {
      case q"package $ref { ..$topstats }" => q"package $ref { ..${stagedObj :: topstats} }"
      case _ => orig
    }
*/
    orig
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
      new ScalanPluginComponent(global)
    )

    result.toList
  }
}
