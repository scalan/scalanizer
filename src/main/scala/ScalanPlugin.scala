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
      case "save-meta" => ScalanPlugin.saveMeta = true
      case "read-meta" => ScalanPlugin.readMeta = true
      case option => error("Option not understood: " + option)
    }
  }

  /** A description of the plugin's options */
  override val optionsHelp = Some(
    "  -P:"+ name +":save-meta     Save META boilerplate to source files.\n"+
    "  -P:"+ name +":read-meta     Read META boilerplate from source files.\n")
}

object ScalanPlugin {
  var readMeta: Boolean = false
  var saveMeta: Boolean = false

  /** Mapping of CakeSlice to user's extension traits */
  val emap = scala.collection.mutable.Map[String, Set[String]]()

  /** Yields the list of Components to be executed in this plugin */
  def components(global: Global) = {
    List(
      new ScalanImport(global),
      new AddAnnot(global, emap),
      new CheckExt(global, emap),
      new AddExt(global, emap),
      new Printer(global)
    )
  }
}
