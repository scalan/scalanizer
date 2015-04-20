package scalan.plugin

import java.io.File
import ScalanAst._

trait ScalanUtils { self: ScalanPluginCake =>
  /** Imports scalan._ and other packages needed by Scalan and further transformations. */
  def addImports(module: SEntityModuleDef) = {
    module.copy(imports = List(
      SImportStat("scalan._")
    ))
  }

  /** Introduces a synonym for a module. */
  def repSynonym(module: SEntityModuleDef) = {
    val entity = module.entityOps
    module.copy(entityRepSynonym = Some(STpeDef(
      name = "Rep" + entity.name,
      tpeArgs = entity.tpeArgs,
      rhs = STraitCall("Rep", List(STraitCall(entity.name, entity.tpeArgs.map(_.toTraitCall))))
    )))
  }

  /** Extends the entiry T by Reifiable[T] */
  def addAncestors(module: SEntityModuleDef) = {
    val newAncestors = STraitCall(
      name = "Reifiable",
      tpeSExprs = List(STraitCall(module.entityOps.name, List()))
    ) :: module.entityOps.ancestors
    val newEntity = module.entityOps.copy(ancestors = newAncestors)

    module.copy(entityOps = newEntity, entities = List(newEntity))
  }

  /** Puts the module to the cake. For example, trait Segments is transformed to
    * trait Segments {self: SegmentsDsl => ... } */
  def updateSelf(module: SEntityModuleDef) = {
    module.copy(selfType = Some(SSelfTypeDef(
      name = "self",
      components = List(STraitCall(module.name + "Dsl", List()))
    )))
  }

  /** Defines a default elem for the entity */
  def addDefaultElem(module: SEntityModuleDef) = {
    val entityName = module.entityOps.name
    val newMethods = SMethodDef(
      name = "default" + entityName + "Elem",
      tpeArgs = List[STpeArg](),
      argSections = List[SMethodArgs](),
      tpeRes = Some(STraitCall("Elem", List(STraitCall(entityName, List())))),
      isImplicit = true,
      overloadId = None
    ) :: module.methods

    module.copy(methods = newMethods)
  }

  def createCompanion(baseName: String) = STraitDef(
    name = baseName + "Companion",
    tpeArgs = List(),
    ancestors = List(),
    body = List(),
    selfType = None,
    companion = None
  )

  /** Checks that the entity has a companion. If the entity doesn't have it
    * then the method adds the companion. */
  def checkEntityCompanion(module: SEntityModuleDef) = {
    val entity = module.entityOps
    val newCompanion = entity.companion match {
      case c @ Some(_) => c
      case None => Some(createCompanion(entity.name))
    }
    val newEntity = entity.copy(companion = newCompanion)

    module.copy(entityOps = newEntity, entities = List(newEntity))
  }

  /** Checks that concrete classes have their companions. */
  def checkClassCompanion(module: SEntityModuleDef) = {
    val newClasses = module.concreteSClasses.map{ clazz =>
      val newCompanion = clazz.companion match {
        case c @ Some(_) => c
        case None => Some(createCompanion(clazz.name))
      }

      clazz.copy(companion = newCompanion)
    }

    module.copy(concreteSClasses = newClasses)
  }

  def saveImplCode(file: File, implCode: String) = {
    val fileName = file.getName.split('.')(0)
    val folder = file.getParentFile
    val implFile = FileUtil.file(folder, "impl", s"${fileName}Impl.scala")

    implFile.mkdirs()

    FileUtil.write(implFile, implCode)
  }

  def eraseModule(module: SEntityModuleDef): SEntityModuleDef = {
    val classes = module.concreteSClasses.map(clazz =>
      clazz.copy(body = clazz.body.map(_ match {
        case m: SMethodDef => m.copy(body = None)
        case item => item
      }))
    )

    module.copy(concreteSClasses = classes)
  }
}
