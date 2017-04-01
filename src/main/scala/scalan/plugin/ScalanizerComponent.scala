package scalan.plugin

import scala.tools.nsc.plugins.PluginComponent
import scalan.meta.scalanizer.{ScalanizerConfig, ScalanizerBase, ScalanizerState}
import scala.tools.nsc.Global

/**
  * Created by slesarenko on 01/04/17.
  */
abstract class ScalanizerComponent(val plugin: ScalanPlugin)
       extends PluginComponent with ScalanizerBase {
  val global: Global = plugin.global
  def snState : ScalanizerState = plugin.snState
  def snConfig: ScalanizerConfig = plugin.snConfig
}
