package scalan.plugin

trait HotSpots {

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
          val params = vparamss.flatten.map{v => Ident(v.name)}
          val kernelInvoke = q"$packageName.HotSpotKernels.$kernelName(..$params)"

          method.copy(rhs = kernelInvoke)
        case _ => super.transform(tree)
      }
    }

    hotSpotTransformer.transform(unitBody)
  }
}
