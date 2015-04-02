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

  
}
