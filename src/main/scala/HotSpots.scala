package scalan.plugin

trait HotSpots extends Enricher with Backend {

  type Compiler <: scala.tools.nsc.Global
  val compiler: Compiler
  import compiler._

  def transformHotSpots(moduleName: String, unitBody: Tree): Tree = {
    val hotSpotTransformer = new Transformer {
      def isHotSpot(annotations: List[Tree]): Boolean = {
        annotations.exists(annotation => annotation match {
          case Apply(Select(New(Ident(TypeName("HotSpot"))), termNames.CONSTRUCTOR), List()) => true
          case _ => false
        })
      }
      override def transform(tree: Tree): Tree = tree match {
        case method @ DefDef(_, TermName(name), _, vparamss, _,_)  if isHotSpot(method.mods.annotations) =>
          val kernelName = TermName(name + "Kernel")
          val packageName = TermName("implOf" + moduleName)
          val params = vparamss.map(_.map{v => Ident(v.name)})
          val kernelInvoke = q"$packageName.HotSpotKernels.$kernelName(...$params)"

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
    val hotSpotNames = List("ddmvm")
    val wrappers = hotSpotNames.map { hotSpotName =>
      q"""
      lazy val ${TermName(hotSpotName + "Wrapper")} = fun(((in: Rep[scala.Tuple2[Array[Array[Double]], Array[Double]]]) => {
        LA.${TermName(hotSpotName)}(in._1, in._2)
      }))
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
