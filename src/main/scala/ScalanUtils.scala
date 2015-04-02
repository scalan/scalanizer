package scalan.plugin

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
}
