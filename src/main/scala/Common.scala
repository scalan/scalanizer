package scalan.plugin

import scalan.meta.ScalanAst._

trait Common {
  /** Converts the name of external type to the name of its wrapper. */
  def wrap(name: String) = "W" + name
  /** Converts the name of external type to the name of the module which
    * contains a wrapper for the type. */
  def wmod(name: String) = "W" + name + "s"
  /** Gets name of companion by entity name */
  def comp(name: String) = name + "Companion"

  /** The class implements a default Meta AST transformation strategy: breadth-first search */
  class MetaAstTransformer {
    def methodArgTransform(arg: SMethodArg): SMethodArg = arg
    def methodArgsTransform(args: SMethodArgs): SMethodArgs = {
      val newArgs = args.args mapConserve methodArgTransform

      args.copy(args = newArgs)
    }
    def methodArgSectionsTransform(argSections: List[SMethodArgs]): List[SMethodArgs] = {
      argSections mapConserve methodArgsTransform
    }
    def methodResTransform(res: Option[STpeExpr]): Option[STpeExpr] = res
    def methodTransform(method: SMethodDef): SMethodDef = {
      val newArgSections = methodArgSectionsTransform(method.argSections)
      val newTpeRes = methodResTransform(method.tpeRes)

      method.copy(argSections = newArgSections, tpeRes = newTpeRes)
    }
    def bodyItemTransform(bodyItem: SBodyItem): SBodyItem = bodyItem match {
      case method: SMethodDef => methodTransform(method)
      case _ => bodyItem
    }
    def bodyTransform(body: List[SBodyItem]): List[SBodyItem] = body mapConserve bodyItemTransform

    def traitCompTransform(traitComp: STraitDef): STraitDef = {
      val newBody = bodyTransform(traitComp.body)

      traitComp.copy(body = newBody)
    }
    def entityCompTransform(companion: Option[STraitOrClassDef]): Option[STraitOrClassDef] = {
      companion match {
        case Some(tr: STraitDef) => Some(traitCompTransform(tr))
        case _ => companion
      }
    }
    def entityTransform(entity: STraitDef): STraitDef = {
      val newBody = bodyTransform(entity.body)
      val newCompanion = entityCompTransform(entity.companion)

      entity.copy(body = newBody, companion = newCompanion)
    }

    def classCompanionTransform(companion: Option[STraitOrClassDef]): Option[STraitOrClassDef] = {
      companion.map {_ match {
        case obj: SObjectDef => obj.copy(body = bodyTransform(obj.body))
        case tr: STraitDef => tr.copy(body = bodyTransform(tr.body))
        case unknown => throw new NotImplementedError(unknown.toString)
      }}
    }
    def classArgTransform(classArg: SClassArg): SClassArg = classArg
    def classArgsTransform(classArgs: SClassArgs): SClassArgs = {
      val newArgs = classArgs.args mapConserve classArgTransform

      classArgs.copy(args = newArgs)
    }
    def classTransform(clazz: SClassDef): SClassDef = {
      val newCompanion = classCompanionTransform(clazz.companion)
      val newClassArgs = classArgsTransform(clazz.args)

      clazz.copy(args = newClassArgs, companion = newCompanion)
    }

    def moduleTransform(module: SEntityModuleDef): SEntityModuleDef = {
      val newEntityOps = entityTransform(module.entityOps)
      val newEntities = module.entities mapConserve entityTransform
      val newClasses = module.concreteSClasses mapConserve classTransform

      module.copy(
        entityOps = newEntityOps,
        entities = newEntities,
        concreteSClasses = newClasses
      )
    }
  }

  class ExtType2WrapperTransformer(name: String) extends MetaAstTransformer {
    override def methodArgTransform(arg: SMethodArg): SMethodArg = arg match {
      case marg @ SMethodArg(_,_,_,STraitCall(tname, params),_,_,_) if tname == name =>
        marg.copy(tpe = STraitCall(wrap(name), params))
      case _ => arg
    }
    override def methodResTransform(res: Option[STpeExpr]): Option[STpeExpr] = res match {
      case Some(STraitCall(tname, tparams)) if tname == name =>
        Some(STraitCall(wrap(name), tparams))
      case _ => res
    }
    override def classArgTransform(classArg: SClassArg): SClassArg = classArg match {
      case arg @ SClassArg(_,_,_,_,STraitCall(tname, params),_,_,_) if tname == name =>
        arg.copy(tpe = STraitCall(wrap(name), params))
      case _ => classArg
    }
  }

  /** Removing of internal parts of annotations that should be ignored at code generation. */
  def filterInternalAnnot(annotations: List[SAnnotation]): List[SAnnotation] = {
    annotations map {_ match {
      case annotation @ SMethodAnnotation("Constructor", args) =>
        val newArgs = args filter {_ match {
          case SAssign(SIdent("original"), m: SMethodDef) => false
          case _ => true
        }}
        annotation.copy(args = newArgs)
      case other => other
    }}
  }
}
