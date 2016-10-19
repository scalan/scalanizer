package scalan.plugin

import scalan.util.FileUtil
import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{TypingTransformers, Transform}
import scalan.meta.ScalanAst._
import scalan.meta.ScalanCodegen

object VirtBackend {
  val name = "scalan-virt-backend"
}

/** Generating of Scala AST for wrappers.
  * (The trick is described here http://stackoverflow.com/questions/7797769)
  * */
class VirtBackend(val global: Global)
    extends PluginComponent with TypingTransformers with Transform
    with Enricher with Backend {
  import global._

  import ScalanPluginState._

  val phaseName: String = VirtBackend.name
  override def description: String = "Generating of Scala AST for virtualized cake."
  override val runsAfter = List(WrapBackend.name)

  def newTransformer(unit: CompilationUnit) = new VirtBackendTransformer(unit)

  var additionalClasses: List[Tree] = Nil

  lazy val topPackage = mkTopLevelPackage("scalanizer.linalgebra")

  /** Create a new package symbol inside the empty (default) package. Takes
    *  care of wiring the module, module class and their types. */
  def mkTopLevelPackage(name: String): Symbol = {
    val pkg = definitions.ScalaPackageClass.newPackage(TermName(name), NoPosition)
    pkg.moduleClass setInfo ClassInfoType(Nil, newScope, pkg.moduleClass)
    pkg.setInfo(pkg.moduleClass.tpe)
    pkg
  }

  class VirtBackendTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case PackageDef(ns, stats) =>
        val unitName = unit.source.file.name
        if (unitName == "LinearAlgebra.scala") {
          val stats1 = atOwner(tree.symbol.moduleClass)(transformStats(stats, currentOwner))
          val res = mkCombinedCake2(ns, topPackage, "LinearAlgebra", tree, stats1)
          saveDebugCode(unitName, showCode(res))
          res
        }
        else
          tree
      // so that tree is actually traversed
      case _ =>
        super.transform(tree)
    }

    def mkCombinedCake(ns: RefTree, topPackage: Symbol, name: String, originalTree: Tree, originalStats: List[Tree]): Tree = {
      implicit val genCtx = GenCtx(module = null, toRep = false)
      //    val ns = genRefs(namespace.split('.').toList).asInstanceOf[RefTree]
      val imports = List(q"import scalan._")
      val absCake = q"""
                  trait ${TypeName(name + "Dsl")}
                    extends Scalan with WrappersDsl
                    """
      val stdCake = q"""
                  trait ${TypeName(name + "DslStd")}
                    extends WrappersDslStd with ${TypeName(name + "Dsl")}
                  """
      val expCake = q"""
                  trait ${TypeName(name + "DslExp")}
                    extends WrappersDslExp
                    with ${TypeName(name + "Dsl")}
                  """
      val objectSE = q"object StagedEvaluation {..${imports ++ List(absCake, stdCake, expCake)}}"
      val implPackage = PackageDef(
        Ident(TermName("implOf"+name)),
        List(
          q"import wrappers._",
          objectSE)
      )
      val pkgTree = PackageDef(Ident(topPackage), List(implPackage)).setSymbol(topPackage)
      val cake = treeCopy.PackageDef(
                  originalTree, ns,
                  originalStats :+ localTyper.typed(pkgTree))
      cake
    }

    def mkCombinedCake2(ns: RefTree, topPackage: Symbol, name: String, originalTree: Tree, originalStats: List[Tree]): Tree = {
      implicit val genCtx = GenCtx(module = null, toRep = false)
      //    val ns = genRefs(namespace.split('.').toList).asInstanceOf[RefTree]
      val imports = List(
        q"import scalan._",
        q"import wrappers._")
      val absCake = q"""
                  import scalan._
                  import wrappers._
                  trait ${TypeName(name + "Dsl")}
                    extends Scalan with WrappersDsl
                    """

      additionalClasses ::= doTypeing(unit, absCake)
//      val pkgTree = PackageDef(Ident(topPackage), additionalClasses).setSymbol(topPackage)
      val cake = treeCopy.PackageDef(
        originalTree, ns,
        originalStats :+ absCake)
      cake
    }

    def doTypeing(unit: CompilationUnit, t: Tree): Tree = {
      t.clearType()
      val ctx = analyzer.rootContext(unit)
      analyzer.newNamer(ctx).enterSym(t)
      val typer = analyzer.newTyper(ctx)
      val tt = typer.typed(t)
      tt
    }

    def mkCombinedCake3(ns: RefTree, topPackage: Symbol, name: String, originalTree: Tree, originalStats: List[Tree]): Tree = {
      implicit val genCtx = GenCtx(module = null, toRep = false)
      //    val ns = genRefs(namespace.split('.').toList).asInstanceOf[RefTree]
      val imports = List(
        q"import scalan._",
        q"import wrappers._")
      val absCake = q"""
                  trait ${TypeName(name + "Dsl")}
                    extends Scalan with WrappersDsl
                    """
      val stdCake = q"""
                  trait ${TypeName(name + "DslStd")}
                    extends WrappersDslStd with ${TypeName(name + "Dsl")}
                  """
      val expCake = q"""
                  trait ${TypeName(name + "DslExp")}
                    extends WrappersDslExp
                    with ${TypeName(name + "Dsl")}
                  """
      val objectSE = q"object StagedEvaluation {..${imports ++ List(absCake, stdCake, expCake)}}"

      val cake = treeCopy.PackageDef(
        originalTree, ns,
        originalStats :+ objectSE)

      cake.clearType()
      val ctx = analyzer.rootContext(unit)
      analyzer.newNamer(ctx).enterSym(cake)
      val typer = analyzer.newTyper(ctx)
      val typedSE = typer.typed(cake)
      typedSE
    }
  }



  def getCombinedCakeHome(namespace: String) = {
    val namespacePath = namespace.split('.').mkString("/")
    s"${ScalanPluginConfig.home}/src/main/scala/$namespacePath/impl"
  }

  /** Puts all modules to the cakes <name>Dsl, <name>DslStd and <name>DslExp.
    * Stores them into the file: <home>/impl/<name>Impl.scala */
  def saveCombinedCake(namespace: String, name: String, cake: Tree): Unit = {
    saveCombinedCake(namespace, name, showCode(cake))
  }
  def saveCombinedCake(namespace: String, name: String, cake: String): Unit = {
    val cakeFile = FileUtil.file(getCombinedCakeHome(namespace), name + "Impl.scala")
    cakeFile.mkdirs()
    FileUtil.write(cakeFile, cake)
  }
}
