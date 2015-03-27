package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.collection.mutable.Map

/** The component searches user's extensions traits */
class CheckExtension(val global: Global, emap: Map[String, Set[String]]) extends PluginComponent {
  import global._

  val phaseName: String = "scalan-check"
  override def description: String = "Find user's extensions"

  val runsAfter = List[String]("scalan-annot")
  override val runsRightAfter: Option[String] = Some("scalan-annot")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      newTraverser().traverse(unit.body)
    }
  }

  def newTraverser(): Traverser = new ForeachTreeTraverser(checkExtTrait)

  def getTraitName(tpname: TypeName): String = tpname match {
    case TypeName(name) => name
    case _ => ""
  }

  def getCakeSliceName(traitName: String): String = {
    if (traitName.endsWith("Dsl")) traitName.stripSuffix("Dsl")
    else if (traitName.endsWith("DslSeq")) traitName.stripSuffix("DslSeq")
    else if (traitName.endsWith("DslExp")) traitName.stripSuffix("DslExp")
    else ""
  }

  def checkExtTrait(tree: Tree): Unit = tree match {
    case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
      val traitName = getTraitName(tpname)
      val cakeName = getCakeSliceName(traitName)
      emap get cakeName match {
        case Some(s) =>
          //print("Extension is found: " + traitName + " for the cake slice: " + cakeName)
          emap(cakeName) -= traitName
          ()
        case None => ()
      }
    case _ => ()
  }
}
