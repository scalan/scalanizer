package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.{PluginComponent, Plugin}

object ScalanPluginConfig {
  var saveMeta: Boolean = false
  var readMeta: Boolean = false
  var debug: Boolean = false
}

class ScalanPluginComponent(val global: Global) extends PluginComponent {
  import global._

  val phaseName: String = "scalan"
  override def description: String = "Code virtualization and specialization"

  val runsAfter = List[String]("parser")
  override val runsRightAfter: Option[String] = Some("parser")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {

    }
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
      case "save-meta" => ScalanPluginConfig.saveMeta = true
      case "read-meta" => ScalanPluginConfig.readMeta = true
      case "debug"     => ScalanPluginConfig.debug = true
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
