package scalan.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{PluginComponent, Plugin}

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
      case "debug" => println("Debug is enabled.")
      case option => error("Option not understood: " + option)
    }
  }

  /** A description of the plugin's options */
  override val optionsHelp = Some(
    "  -P:"+ name +":debug     output debug information")
}

object ScalanPlugin {
  /** Yields the list of Components to be executed in this plugin */
  def components(global: Global) = {
    List(
      //new HelloComponent(global),
      //new TraverseAnnot(global)
      //new AddAnnot(global)
      new ScalanImport(global)
    )
  }
}
