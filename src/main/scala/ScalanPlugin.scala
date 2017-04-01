package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scalan.meta.scalanizer.{ScalanizerBase, ScalanizerConfig, ScalanizerState}

class ScalanPlugin(val global: Global) extends Plugin {
  val snState: ScalanizerState = ScalanPluginState
  val snConfig: ScalanizerConfig = ScalanPluginConfig

  /** Visible name of the plugin */
  val name: String = "scalan"

  /** The compiler components that will be applied when running this plugin */
  val components: List[PluginComponent] = List(
    new WrapFrontend(this)
    , new WrapEnricher(this)
    , new WrapBackend(this)
    , new VirtBackend(this)
    , new CheckExtensions(this)
    , new FinalComponent(this)
    //      ,new Debug(this)
  )

  /** The description is printed with the option: -Xplugin-list */
  val description: String = "Optimization through staging"

  /** Plugin-specific options without -P:scalan:  */
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

object ScalanPlugin {
  /** Yields the list of Components to be executed in this plugin */
  def components(global: Global) = {
    val plugin = new ScalanPlugin(global)
    plugin.components
  }
}
