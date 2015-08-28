package scalan.plugin

import scalan.compilation.KernelTypes
import scalan.meta.ScalanAst._
import scalan.meta.ScalanParsers

trait HotSpots extends Enricher with Backend with ScalanParsers {

  type Compiler <: scala.tools.nsc.Global
  val compiler: Compiler
  import compiler._

  import KernelTypes._

  /** Mapping of a module to its hot spots. */
  val hotSpots = scala.collection.mutable.Map[String, List[HotSpotMethod]]()

  case class HotSpotMethod(name: String, path: String, vparamss: List[List[ValDef]], res: Tree, kernel: KernelType)
  {
    def identss: List[List[Ident]] = vparamss.map(_.map{v => Ident(v.name)})
    def sparamss: List[List[SValDef]] = vparamss.map(_.map{vd =>
      val tpeRes = optTpeExpr(vd.tpt)
      val isImplicit = vd.mods.isImplicit
      val isLazy = vd.mods.isLazy

      SValDef(vd.name, tpeRes, isLazy, isImplicit, parseExpr(vd.rhs))
    })
    def toLambda: Tree = {
      val body = q"${TermName(path)}.${TermName(name)}(...${identss})"
      genFunc(SFunc(sparamss.flatten, parseExpr(body)))(GenCtx(null, true))
    }
    def typeExpr: Tree = {
      val argTpeExprs = sparamss.map(_.map(_.tpe.getOrElse(STpeEmpty()))).flatten
      val domainTpeExpr = if (argTpeExprs.length == 1) argTpeExprs.head else STpeTuple(argTpeExprs)
      genTypeExpr(STpeFunc(domainTpeExpr, tpeExpr(res)))(GenCtx(null, false))
    }
  }

  def transformHotSpots(module: SEntityModuleDef, unitBody: Tree): Tree = {
    val hotSpotTransformer = new Transformer {
      def isHotSpot(annotations: List[Tree]): Boolean = {
        annotations.exists(annotation => annotation match {
          case Apply(Select(New(Ident(TypeName("HotSpot"))), termNames.CONSTRUCTOR), _) => true
          case _ => false
        })
      }
      override def transform(tree: Tree): Tree = tree match {
        case method @ DefDef(_, TermName(name), _, vparamss,tpt,_)  if isHotSpot(method.mods.annotations) =>
          val kernelName = TermName(name + "Kernel")
          val packageName = TermName("implOf" + module.name)
          val params = vparamss.map(_.map{v => Ident(v.name)})
          val kernelInvoke = q"$packageName.HotSpotKernels.$kernelName(...$params)"

          hotSpots(module.name) = HotSpotMethod(name, "LA", vparamss, tpt, getKernel(method.mods.annotations)) ::
                                  hotSpots.getOrElse(module.name, Nil)
          method.copy(rhs = kernelInvoke)
        case _ => super.transform(tree)
      }
    }

    hotSpotTransformer.transform(unitBody)
  }

  def getHotSpotKernels(module: SEntityModuleDef) = {
    val kernels = hotSpots.getOrElse(module.name, Nil).map { method =>
      val scalanContextGetter = method.kernel match {
        case CppKernel => "getScalanContextUni"
        case _ => "getScalanContext"
      }
      q"""
        lazy val ${TermName(method.name + "Kernel")} = {
          val ctx = HotSpotManager.${TermName(scalanContextGetter)}
          val config = new ctx.CompilerConfig(Some("2.11.2"), Seq.empty)
          val compilerOutput = ctx.buildExecutable(
            new File("./it-out/" + ${method.name}),
            ${Literal(Constant(method.name))},
            ctx.scalan.${TermName(method.name + "Wrapper")}, GraphVizConfig.default)(config)
          val (cls, method) = ctx.loadMethod(compilerOutput)
          val instance = cls.newInstance().asInstanceOf[${method.typeExpr}]
          instance
        }
       """
    }
    q"""
      object HotSpotKernels {
        import java.io.File
        import scalan.compilation.GraphVizConfig
        import scala.language.reflectiveCalls

        ..$kernels
      }
    """
  }

  def getHotSpotManager(module: SEntityModuleDef) = {
    val cakeName = getCakeName(module)
    val wrappers = hotSpots.getOrElse(module.name, Nil).map { method =>
      (method, q"lazy val ${TermName(method.name + "Wrapper")} = ${method.toLambda}")
    }.partition{w => w._1.kernel == ScalaKernel}
    val ScalaWrappers = wrappers._1.map(_._2)
    val CppWrappers = wrappers._2.map(_._2)
    implicit val ctx = GenCtx(null, false)
    val cakeImport = genImport(getImportByName(cakeName))

    q"""
      object HotSpotManager {
        import scalan.ScalanCommunityDslExp;
        import scalan.compilation.lms.scalac.CommunityLmsCompilerScala;
        import scalan.{CommunityMethodMappingDSL, JNIExtractorOpsExp}
        import scalan.compilation.lms.CommunityBridge
        $cakeImport

        lazy val prog = new ${TypeName(cakeName+"DslExp")} with ScalanCommunityDslExp with JNIExtractorOpsExp {
          ..$ScalaWrappers
        }
        lazy val compiler = new CommunityLmsCompilerScala(prog) with CommunityBridge with CommunityMethodMappingDSL;
        def getScalanContext = compiler;

        import scalan.compilation.lms.uni.LmsCompilerUni;
        lazy val progUni = new ${TypeName(cakeName +"DslExp")} with ScalanCommunityDslExp with JNIExtractorOpsExp {
          ..$CppWrappers
        }
        lazy val compilerUni = new LmsCompilerUni(progUni) with CommunityBridge with CommunityMethodMappingDSL;
        def getScalanContextUni = compilerUni;
      }
    """
  }

  def getCakeName(module: SEntityModuleDef) = module.selfType match {
    case Some(SSelfTypeDef(_, List(STraitCall(name, _)))) => name
    case _ => module.name
  }

  def getKernel(annotations: List[Tree]): KernelType = {
    val annotArgs = annotations.collectFirst {
      case Apply(Select(New(Ident(TypeName("HotSpot"))), termNames.CONSTRUCTOR), args) => args
    }

    annotArgs match {
      case Some(List(Ident(TermName("CppKernel")))) => CppKernel
      case Some(List(Ident(TermName("ScalaKernel")))) => ScalaKernel
      case _ => ScalaKernel
    }
  }
}
