package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scalan.meta.ScalanAst._
import scalan.meta.{CodegenConfig, ScalanParsers}

/** The component builds wrappers. */
class Wrapping(val global: Global) extends PluginComponent with ScalanParsers {

  type Compiler = global.type
  val compiler: Compiler = global
  import compiler._

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
    def formMethodDef(name: String, args: List[Symbol], res: Type): SMethodDef = {
      val methodArgs = args.map{arg =>
        SMethodArg(
          impFlag = false, overFlag = false,
          name = arg.toString,
          tpe = parseType(arg.tpe),
          default = None, annotations = Nil, isElemOrCont = false
        )
      }
      SMethodDef(
        name = name,
        tpeArgs = Nil,
        argSections = List(SMethodArgs(methodArgs)),
        tpeRes = Some(parseType(res)),
        isImplicit = false, isOverride = false,
        overloadId = None,
        annotations = List(SMethodAnnotation(annotationClass = "External", args = Nil)),
        body = None,
        isElemOrCont = false
      )
    }
    val member = memberType match {
      case MethodType(args, res) => formMethodDef(memberName.toString, args, res)
      case TypeRef(_,sym,_) => formMethodDef(memberName.toString, Nil, sym.tpe)
      case _ => throw new NotImplementedError(s"memberType = ${showRaw(memberType)}")
    }
    val wrapperName = "S" + externalType.nameString

    STraitDef(
      name = wrapperName,
      tpeArgs = Nil,
      ancestors = List(STraitCall(
        "TypeWrapper",
        List(STraitCall(externalType.nameString, Nil), STraitCall(wrapperName, Nil))
      )),
      body =  List[SBodyItem](member),
      selfType = Some(SSelfTypeDef("self", Nil)),
      companion = None, annotations = Nil
    )
  }

  def config: CodegenConfig = ScalanPluginConfig.codegenConfig
}

