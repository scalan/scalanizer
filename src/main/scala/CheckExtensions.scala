package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.collection.mutable.Map

/** The component searches user's extensions traits */
class CheckExtensions(val global: Global) extends PluginComponent {
  import global._

  val phaseName: String = "scalan-check"
  override def description: String = "Find user's extensions"

  val runsAfter = List[String]("parser")
  override val runsRightAfter: Option[String] = Some("parser")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      val hasStagedObj = unit.body match {
        case q"package $ref0 { package $ref1 {..$topstats }}" =>
          topstats.exists(stat => stat match {
            case q"object StagedEvaluation {..$stats}" => true
            case _ => false
          })
        case _ => false
      }
      if (!hasStagedObj)
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
      ScalanPluginState.extMap get cakeName match {
        case Some(s) =>
          //print("Extension is found: " + traitName + " for the cake slice: " + cakeName)
          ScalanPluginState.extMap(cakeName) -= traitName
          ()
        case None => ()
      }
    case _ => ()
  }
}

