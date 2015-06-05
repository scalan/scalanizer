package scalan.plugin

import scalan.meta.ScalanAst._
import scalan.meta.ScalanParsers

trait HotSpots extends Enricher with Backend with ScalanParsers {

  type Compiler <: scala.tools.nsc.Global
  val compiler: Compiler
  import compiler._

  case class HotSpotMethod(name: String, path: String, vparamss: List[List[ValDef]], res: Tree)
  {
    def identss: List[List[Ident]] = vparamss.map(_.map{v => Ident(v.name)})
    def sparamss: List[List[SValDef]] = vparamss.map(_.map{vd =>
      val tpeRes = optTpeExpr(vd.tpt)
      val isImplicit = vd.mods.isImplicit
      val isLazy = vd.mods.isLazy

      SValDef(vd.name, tpeRes, isLazy, isImplicit, parseExpr(vd.rhs))
    })
    def toLambda = {
      implicit val ctx = GenCtx(null, true)
      val body = q"${TermName(path)}.${TermName(name)}(...${identss})"
      genFunc(SFunc(sparamss.flatten, parseExpr(body)))
    }
  }

  var hotSpots: List[HotSpotMethod] = Nil

  def transformHotSpots(moduleName: String, unitBody: Tree): Tree = {
    val hotSpotTransformer = new Transformer {
      def isHotSpot(annotations: List[Tree]): Boolean = {
        annotations.exists(annotation => annotation match {
          case Apply(Select(New(Ident(TypeName("HotSpot"))), termNames.CONSTRUCTOR), List()) => true
          case _ => false
        })
      }
      override def transform(tree: Tree): Tree = tree match {
        case method @ DefDef(_, TermName(name), _, vparamss,tpt,_)  if isHotSpot(method.mods.annotations) =>
          val kernelName = TermName(name + "Kernel")
          val packageName = TermName("implOf" + moduleName)
          val params = vparamss.map(_.map{v => Ident(v.name)})
          val kernelInvoke = q"$packageName.HotSpotKernels.$kernelName(...$params)"

          hotSpots = HotSpotMethod(name, "LA", vparamss, tpt) :: hotSpots
          method.copy(rhs = kernelInvoke)
        case _ => super.transform(tree)
      }
    }

    hotSpotTransformer.transform(unitBody)
  }

  def getHotSpotKernels = {
    val hotSpotNames = List("ddmvm")
    val kernels = hotSpotNames.map{hotSpotName =>
      q"""
        lazy val ${TermName(hotSpotName + "Kernel")} = {
          val ctx = HotSpotManager.getScalanContext
          val compilerOutput = ctx.buildExecutable(
            new File("./"),
            ${Literal(Constant(hotSpotName))},
            ctx.${TermName(hotSpotName + "Wrapper")}, GraphVizConfig.default)(ctx.defaultCompilerConfig)
          val (cls, method) = ctx.loadMethod(compilerOutput)
          val instance = cls.newInstance().asInstanceOf[((Array[Array[Double]], Array[Double])) => Array[Double]]
          instance
        }
       """
    }
    q"""
      object HotSpotKernels {
        import java.io.File
        import scalan.compilation.GraphVizConfig
        ..$kernels
      }
    """
  }

  def getHotSpotManager = {
    val cakeName = "LinearAlgebra"
    val wrappers = hotSpots.map { method =>
      q"""
      lazy val ${TermName(method.name + "Wrapper")} = ${method.toLambda}
      """
    }
    implicit val ctx = GenCtx(null, false)
    val cakeImport = genImport(getImportByName(cakeName))
    q"""
      object HotSpotManager {
        import scalan.ScalanCommunityDslExp
        import scalan.compilation.lms.{CommunityLmsBackend, CoreBridge}
        import scalan.compilation.lms.scalac.CommunityLmsCompilerScala
        import scalan.primitives.EffectfulCompiler
        $cakeImport

        lazy val scalanContext = new Scalan
        def getScalanContext = scalanContext

        class Scalan extends ${TypeName(cakeName+"DslExp")} with CommunityLmsCompilerScala with CoreBridge
          with ScalanCommunityDslExp with EffectfulCompiler {

          ..$wrappers
          val lms = new CommunityLmsBackend
        }
      }
    """
  }
}
