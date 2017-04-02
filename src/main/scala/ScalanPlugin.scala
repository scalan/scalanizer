package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scalan.meta.scalanizer.{ScalanizerConfig, ScalanizerBase, ScalanizerState, Scalanizer}

class ScalanPlugin(val global: Global) extends Plugin { plugin =>
  val scalanizer: Scalanizer[plugin.global.type] = new Scalanizer[plugin.global.type] {
    def getGlobal: plugin.global.type = plugin.global
    val snState: ScalanizerState[plugin.global.type] = new ScalanPluginState(this)
    val snConfig: ScalanizerConfig = new ScalanPluginConfig
  }

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
      case "save" => scalanizer.snConfig.withSave(true)
      case "read" => scalanizer.snConfig.withRead(true)
      case "debug" => scalanizer.snConfig.withDebug(true)
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
