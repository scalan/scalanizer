package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scalan.meta.ScalanAst._

/** The component builds wrappers. */
class Wrapping(val global: Global) extends PluginComponent {
  import global._

  val phaseName: String = "scalan-wrapping"
  override def description: String = "Building wrappers for external types"

  val runsAfter = List[String]("typer")
  override val runsRightAfter: Option[String] = Some("typer")

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      if (Set("Cols.scala").contains(unit.source.file.name)) {
        newTraverser().traverse(unit.body)
      }
    }
  }

  def newTraverser(): Traverser = new ForeachTreeTraverser(catchWrapperUsage)

  def catchWrapperUsage(tree: Tree): Unit = tree match {
    case sel @ Select(objSel @ Select(_, obj), member) if isWrapper(objSel.tpe.typeSymbol) =>
      val wrapper = updateWrapper(objSel.tpe.typeSymbol, member, sel.tpe)
      print(wrapper)
    case _ => ()
  }

  def isWrapper(sym: Symbol): Boolean = {
    Set("Arr").contains(sym.nameString)
  }

  def updateWrapper(externalType: Symbol, memberName: Name, memberType: Type): STraitDef = {
    val member = memberType match {
      case MethodType(args, res) =>
        SMethodDef(
          name = memberName.toString,
          tpeArgs = Nil, argSections = Nil, tpeRes = None,
          isImplicit = false, isOverride = false,
          overloadId = None, annotations = Nil, body = None,
          isElemOrCont = false
        )
      case TypeRef(_,sym,_) =>
        SValDef(
          name = memberName.toString,
          tpe = Some(STpePrimitive(sym.tpe.toString, "")),
          isLazy = false, isImplicit = false,
          expr = SEmpty()
        )
      case _ => throw new NotImplementedError(s"memberType = ${showRaw(memberType)}")
    }

    STraitDef(
      name = externalType.nameString,
      tpeArgs = Nil,
      ancestors = List(STraitCall("TypeWrapper", List(STraitCall(externalType.nameString, List[STpeExpr]())))),
      body =  List[SBodyItem](member),
      selfType = Some(SSelfTypeDef("self", Nil)),
      companion = None, annotations = Nil
    )
  }
}

