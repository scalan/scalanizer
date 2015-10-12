package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class ScalanPlugin(val global: Global) extends Plugin {
  /** Visible name of the plugin */
  val name: String = "scalan"

  /** The compiler components that will be applied when running this plugin */
  val components: List[PluginComponent] = List(
    new WrapFrontend(global)
    , new WrapEnricher(global)
    , new WrapBackend(global)
    , new CheckExtensions(global)
    , new ScalanPluginComponent(global)
    //      ,new Debug(global)
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
