package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scalan.meta.ScalanAst._

/** Virtualization of type wrappers. */
class WrapEnricher(val global: Global) extends PluginComponent with Enricher {

  type Compiler = global.type
  val compiler: Compiler = global

  import compiler._

  val phaseName: String = "scalan-wrap-enricher"

  override def description: String = "Virtualization of type wrappers."

  val runsAfter = List[String]("scalan-wrap-frontend")
  override val runsRightAfter: Option[String] = Some("scalan-wrap-frontend")

  /** The phase prepares a wrapper for virtualization. */
  def newPhase(prev: Phase) = new StdPhase(prev) {
    override def run(): Unit = {
      ScalanPluginState.wrappers transform { (name, module) =>
        /** Transformations of Wrappers by adding of Elem, Cont and other things. */
        val pipeline = scala.Function.chain(Seq(
          addWrappedValue _,
          addModuleAncestors _,
          updateSelf _,
          repSynonym _,
          checkEntityCompanion _,
          constr2apply _,
          filterClassTags _,
          genEntityImpicits _,
          genMethodsImplicits _,
          defaultMethod _,
          defaultWrapperImpl _,
          extType2WrapperInWrappers _
        ))
        val enrichedModule = pipeline(module)

        enrichedModule
      }
    }

    def apply(unit: CompilationUnit): Unit = ()
  }

  /** Adding of a method which return original external type. For example:
    * def wrappedValueOfBaseType: Rep[Array[T]]; */
  def addWrappedValue(module: SEntityModuleDef): SEntityModuleDef = {
    val resType = module.entityOps.ancestors.collect {
      case STraitCall("TypeWrapper", List(importedType, _)) => importedType
    }.headOption
    val wrappedValueOfBaseType = SMethodDef(
      name = "wrappedValueOfBaseType",
      tpeArgs = Nil, argSections = Nil,
      tpeRes = resType,
      isImplicit = false, isOverride = false, overloadId = None,
      annotations = Nil, body = None, isElemOrCont = false
    )
    val updatedEntity = module.entityOps.copy(
      body = wrappedValueOfBaseType :: module.entityOps.body
    )

    module.copy(entityOps = updatedEntity, entities = List(updatedEntity))
  }

  /** Adding of a method which returns default value of external type.
    * For example: def DefaultOfArray[T]: Default[Array[T]] = ???. */
  def defaultMethod(module: SEntityModuleDef): SEntityModuleDef = {
    val extType = module.entityOps.ancestors.collect {
      case STraitCall("TypeWrapper", List(importedType, _)) => importedType
    }
    val defaultOfWrapper = SMethodDef(
      name = "DefaultOf" + extType.head.name,
      tpeArgs = module.entityOps.tpeArgs,
      argSections = Nil,
      tpeRes = Some(STraitCall("Default", extType)),
      isImplicit = false, isOverride = false, overloadId = None,
      annotations = Nil,
      body = Some(SApply(SSelect(SIdent("Default"),"defaultVal"), Nil, List(List(SConst(null))))),
      isElemOrCont = true // Workaround: disable virtualization of the method
    )
    module.copy(methods = defaultOfWrapper :: module.methods)
  }

  /** Adding of default implementation of the type wrapper. It is required by
    * Scalan Codegen. When the module is stored, the default implementation
    * is filtered. */
  def defaultWrapperImpl(module: SEntityModuleDef): SEntityModuleDef = {
    val wrapperType = module.entityOps.ancestors.collect {
      case STraitCall("TypeWrapper", h :: _) => h
    }.head
    val wrapperImpl = SEntityModuleDef.wrapperImpl(module.entityOps, wrapperType)

    module.copy(concreteSClasses = List(wrapperImpl))
  }

  def filterConstructor(module: SEntityModuleDef): SEntityModuleDef = {
    new MetaAstTransformer {
      override def bodyTransform(body: List[SBodyItem]): List[SBodyItem] = body.filter {
        _ match {
          case m: SMethodDef if m.name == "<init>" => false
          case _ => true
        }
      }
    }.moduleTransform(module)
  }

  def constr2apply(module: SEntityModuleDef): SEntityModuleDef = {
    val (constrs, entityBody) = module.entityOps.body partition{ _ match {
      case m: SMethodDef if m.name == "<init>" => true
      case _ => false
    }}
    val applies = constrs collect {
      case c: SMethodDef => c.copy(
        name = "apply",
        tpeArgs = (module.entityOps.tpeArgs ++ c.tpeArgs).distinct,
        // This is an internal annotation. And it should be ignored during in the backend.
        annotations = List(SMethodAnnotation("Constructor", List(SAssign(SIdent("original"), c))))
      )
    }
    val newEntityCompanion = module.entityOps.companion match {
      case Some(companion: STraitDef) => Some(companion.copy(body = applies ++ companion.body))
      case other => other
    }
    val newEntityOps = module.entityOps.copy(body = entityBody, companion = newEntityCompanion)

    module.copy(entityOps = newEntityOps, entities = List(newEntityOps))
  }

  def extType2WrapperInWrappers(module: SEntityModuleDef): SEntityModuleDef = {
    class TypeInWrappersTransformer(name: String) extends ExtType2WrapperTransformer(name) {
      override def methodTransform(method: SMethodDef): SMethodDef = {
        if (method.name == "wrappedValueOfBaseType")
          method
        else super.methodTransform(method)
      }
      override def classArgTransform(classArg: SClassArg) = classArg
      override def entityAncestorTransform(ancestor: STraitCall): STraitCall = {
        if (ancestor.name == "TypeWrapper")
          ancestor
        else
          typeTransformer.traitCallTransform(ancestor)
      }
    }
    val wrappedModule = ScalanPluginConfig.externalTypes.foldLeft(module){(acc, externalTypeName) =>
      new TypeInWrappersTransformer(externalTypeName).moduleTransform(acc)
    }

    wrappedModule
  }
}