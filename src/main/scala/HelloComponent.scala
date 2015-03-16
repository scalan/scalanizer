package scalan.plugin

import scala.tools.nsc.{Phase, Global}
import scala.tools.nsc.plugins.PluginComponent

/** Dummy phase outputs "Hello, World!" */
class HelloComponent(val global: Global) extends PluginComponent {
  val phaseName: String = "hello"

  val runsAfter = List[String]("refchecks")

  def newPhase(prev: Phase) = new Phase(prev) {
    def name = phaseName
    override def description = "Dummy phase"

    def run {
      println("Hello, World! from the phase: " + name)
    }
  }
}
