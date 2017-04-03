package scalan.plugin

import scala.tools.nsc._
import scalan.meta.ScalanAst._

object WrapEnricher {
  val name = "scalanizer-enricher"
}

// TODO ScalanParsers is used only to get wrapperImpl. Move it somewhere?
/** Virtualization of type wrappers. */
class WrapEnricher(override val plugin: ScalanizerPlugin) extends ScalanizerComponent(plugin) {
  import scalanizer._
  import scalanizer.global._

  val phaseName: String = WrapEnricher.name

  override def description: String = "Virtualization of type wrappers."

  val runsAfter = List(WrapFrontend.name)

  /** The phase prepares a wrapper for virtualization. */
  def newPhase(prev: Phase) = new StdPhase(prev) {
    override def run(): Unit = {
      snState.wrappers transform { (name, wrapperDescr) =>
        /** Transformations of Wrappers by adding of Elem, Cont and other things. */
        val pipeline = scala.Function.chain(Seq(
          preventNameConflict _,
          addWrappedValue _,
          addModuleAncestors _,
          updateSelf _,
          repSynonym _,
          checkEntityCompanion _,
          constr2apply _,
          cleanUpClassTags _,
          preventNameConflict _,
          genEntityImpicits _,
          genMethodsImplicits _,
          defaultMethod _,
          defaultWrapperImpl _,
          extType2WrapperInWrappers _,
          /** Currently, inheritance of type wrappers is not supported.
            * Print warnings and remove ancestors. */
          filterAncestors _
        ))
        val enrichedModule = pipeline(wrapperDescr.module)

        wrapperDescr.copy(module = enrichedModule)
      }
    }

    def apply(unit: CompilationUnit): Unit = ()
  }

  /** Adding of a method which return original external type. For example:
    * def wrappedValue: Rep[Array[T]]; */
  def addWrappedValue(module: SModuleDef): SModuleDef = {
    val resType = module.entityOps.ancestors.collect {
      case STraitCall("TypeWrapper", List(importedType, _)) => importedType
    }.headOption
    val wrappedValue = SMethodDef(
      name = "wrappedValue",
      tpeArgs = Nil, argSections = Nil,
      tpeRes = resType,
      isImplicit = false, isOverride = false, overloadId = None,
      annotations = Nil, body = None, isTypeDesc = false
    )
    val updatedEntity = module.entityOps.copy(
      body = wrappedValue :: module.entityOps.body
    )

    module.copy(entityOps = updatedEntity, entities = List(updatedEntity))
  }

  /** Adding of a method which returns default value of external type.
    * For example: def DefaultOfArray[T]: Default[Array[T]] = ???. */
  def defaultMethod(module: SModuleDef): SModuleDef = {
    val baseType = module.entityOps.optBaseType.get
    val tpeArgs = module.entityOps.tpeArgs
    val typeDescs = tpeArgs.map(a =>
          SMethodArg(true, false, "e" + a.name, STraitCall("Elem", List(a.toTraitCall)), None, isTypeDesc = true))
    val defaultOfWrapper = SMethodDef(
      name = "DefaultOf" + module.entityOps.baseInstanceName,
      tpeArgs = tpeArgs,
      argSections = List(SMethodArgs(typeDescs)),
      tpeRes = Some(baseType),
      isImplicit = false, isOverride = false, overloadId = None,
      annotations = Nil,
      body = Some(SConst(null, Some(baseType))),
      isTypeDesc = false // Workaround: disable virtualization of the method
    )
    module.copy(methods = defaultOfWrapper :: module.methods)
  }

  /** Adding of default implementation of the type wrapper. It is required by
    * Scalan Codegen. When the module is stored, the default implementation
    * is filtered. */
  def defaultWrapperImpl(module: SModuleDef): SModuleDef = {
    val wrapperTypes = module.entityOps.ancestors.collect {
      case STraitCall("TypeWrapper", h :: _) => h
    }

    if (wrapperTypes.isEmpty) module
    else {
      val wrapperType = wrapperTypes.head
      val wrapperImpl = this.scalanizer.wrapperImpl(module.entityOps, wrapperType, false)
      module.copy(concreteSClasses = List(wrapperImpl))
    }
  }

  /** Converts constructors (methods with name "<init>") to the apply method of companions. */
  def filterConstructor(module: SModuleDef): SModuleDef = {
    new MetaAstTransformer {
      override def bodyTransform(body: List[SBodyItem]): List[SBodyItem] = body.filter {
        case m: SMethodDef if m.name == "<init>" => false
        case _ => true
      }
    }.moduleTransform(module)
  }

  def constr2apply(module: SModuleDef): SModuleDef = {
    val (constrs, entityBody) = module.entityOps.body partition {
      case m: SMethodDef if m.name == "<init>" => true
      case _ => false
    }
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

  /** Replaces external types by their wrappers. For example:
    * trait Col[A] { def arr: Array[A]; }
    * The external type Array is replaced by its wrapper WArray
    * trait Col[A] { def arr: WArray[A]; }
    * */
  def extType2WrapperInWrappers(module: SModuleDef): SModuleDef = {
    class TypeInWrappersTransformer(name: String) extends ExtType2WrapperTransformer(name) {
      override def methodTransform(method: SMethodDef): SMethodDef = {
        if (method.name == "wrappedValue")
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
    val wrappedModule = snState.externalTypes.foldLeft(module){(acc, externalTypeName) =>
      new TypeInWrappersTransformer(externalTypeName).moduleTransform(acc)
    }

    wrappedModule
  }

  /** Discards all ancestors of the entity except TypeWrapper. It could be used as temporary solution
    * if inheritance of type wrappers is not supported. */
  def filterAncestors(module: SModuleDef): SModuleDef = {
    class filterAncestorTransformer extends MetaAstTransformer {
      override def entityAncestorsTransform(ancestors: List[STraitCall]): List[STraitCall] = {
        ancestors.filter(_.name == "TypeWrapper")
      }
    }

    new filterAncestorTransformer().moduleTransform(module)
  }

  /** Adds a prefix for type parameters To, Elem and Cont, to eliminate name conflicts. */
  def preventNameConflict(module: SModuleDef): SModuleDef = {
    val pipeline = scala.Function.chain(Seq(
      new TypeNameTransformer("Elem", module.name + "Elem").moduleTransform _,
      new TypeNameTransformer("Cont", module.name + "Cont").moduleTransform _,
      new TypeNameTransformer("To", module.name + "To").moduleTransform _
    ))
    val nonConflictModule = pipeline(module)

    nonConflictModule
  }
}