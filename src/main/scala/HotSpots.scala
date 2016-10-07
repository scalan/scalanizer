package scalan.plugin

import scalan.compilation.KernelType
import scalan.meta.ScalanAst._
import scalan.meta.ScalanParsers

trait HotSpots extends Common with Enricher with Backend with ScalanParsers {

  import global._

  import KernelType._

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
      val wrappedParamms = sparamss.flatten map { param =>
        ScalanPluginState.externalTypes.foldLeft(param) { (acc, externalTypeName) =>
          new ExtType2WrapperTransformer(externalTypeName).valdefTransform(acc)
        }
      }
      val body = q"${TermName(path)}.${TermName(name)}(...${identss})"
      genFunc(SFunc(wrappedParamms, parseExpr(body)))(GenCtx(null, true))
    }
    def typeExpr: Tree = {
      val argTpeExprs = sparamss.map(_.map(_.tpe.getOrElse(STpeEmpty()))).flatten
      val domainTpeExpr = if (argTpeExprs.length == 1) argTpeExprs.head else STpeTuple(argTpeExprs)
      genTypeExpr(STpeFunc(domainTpeExpr, tpeExpr(res)))(GenCtx(null, false))
    }
  }

  def transformHotSpots(module: SEntityModuleDef, unit: CompilationUnit): Tree = {
    val hotSpotTransformer = new Transformer {
      def getPackageName = {
        def loop(pkgs: List[String], res: Tree): Tree = (pkgs, res) match {
          case (Nil, _) => res
          case (p :: ps, EmptyTree) => loop(ps, Ident(TermName(p)))
          case (p :: ps, _) => loop(ps, Select(res, TermName(p)))
        }
        val pkgOfModule = ScalanPluginState.packageOfModule(module.name)

        loop(pkgOfModule.split('.').toList :+ ("implOf" + module.name), EmptyTree)
      }
      override def transform(tree: Tree): Tree = tree match {
        case method @ DefDef(_, TermName(name), _, vparamss,tpt,_) if isHotSpotMethod(method) =>
          val kernelName = TermName(name + "Kernel")
          val packageName = getPackageName
          val params = vparamss.map(_.map{v => Ident(v.name)})
          val kernelInvoke = q"$packageName.HotSpotKernels.$kernelName(...$params)"
          val hotspotMethod = HotSpotMethod(
                name, method.symbol.outerClass.nameString, vparamss, tpt,
                getKernel(method.symbol.annotations))
          hotSpots(module.name) =  hotspotMethod :: hotSpots.getOrElse(module.name, Nil)
          /* Inferring of type for the created method. */
          val methodWithKernelInvoke = method.copy(rhs = kernelInvoke).clearType
          val ctx = analyzer.rootContext(unit)
          analyzer.newNamer(ctx).enterSym(methodWithKernelInvoke)
          val methodTyper = analyzer.newTyper(ctx)
          val typedMethod = methodTyper.typedDefDef(methodWithKernelInvoke)

          typedMethod
        case _ => super.transform(tree)
      }
    }

    hotSpotTransformer.transform(unit.body)
    //unit.body
  }

  def getHotSpotKernels(module: SEntityModuleDef): Tree = {
    val kernels = hotSpots.getOrElse(module.name, Nil).map { method =>
      val scalanContextGetter = method.kernel match {
        case Cpp => "getScalanContextUni"
        case _ => "getScalanContext"
      }
      q"""
        lazy val ${TermName(method.name + "Kernel")} = { ??? }
       """
//      q"""
//        lazy val ${TermName(method.name + "Kernel")} = {
//          val ctx = HotSpotManager.${TermName(scalanContextGetter)}
//          val compilerOutput = ctx.buildExecutable(
//            new File("./it-out/" + ${method.name}),
//            ${Literal(Constant(method.name))},
//            ctx.${TermName(method.name + "Wrapper")}, GraphVizConfig.default)(ctx.CompilerConfig(Some("2.11.2"), Seq.empty))
//          val (cls, method) = ctx.loadMethod(compilerOutput)
//          val instance = cls.newInstance().asInstanceOf[${method.typeExpr}]
//          instance
//        }
//       """
    }
    q"""
      object HotSpotKernels {
        import java.io.File
        import scalan.compilation.GraphVizConfig
        ..$kernels
      }
    """
  }

  def getHotSpotManager(module: SEntityModuleDef) = {
    val cakeName = getCakeName(module)
    val wrappers = hotSpots.getOrElse(module.name, Nil).map { method =>
      (method, q"lazy val ${TermName(method.name + "Wrapper")} = ${method.toLambda}")
    }.partition{w => w._1.kernel == Scala}
    val ScalaWrappers = wrappers._1.map(_._2)
    val CppWrappers = wrappers._2.map(_._2)
    implicit val ctx = GenCtx(null, false)
    val cakeImport = genImport(getImportByName(cakeName))
    q"""
      object HotSpotManager {
        import scalan.ScalanDslExp;
        import scalan.compilation.lms.{CoreLmsBackend, CoreLmsBridge};
        import scalan.compilation.lms.scalac.LmsCompilerScala;
        import scalan.primitives.EffectfulCompiler;
        $cakeImport
      }
    """
  }

  def getCakeName(module: SEntityModuleDef) = module.selfType match {
    case Some(SSelfTypeDef(_, List(STraitCall(name, _)))) => name
    case _ => module.name
  }

  def isHotSpotMethod(method: DefDef): Boolean = {
    method.symbol.annotations exists { annotation =>
      annotation.symbol.nameString == "HotSpot"
    }
  }

  def getKernel(annotations: List[AnnotationInfo]): KernelType = {
    annotations.head.args match {
      case Select(_, TermName("CppKernel")) :: _ => Cpp
      case _ => Scala
    }
  }
}
