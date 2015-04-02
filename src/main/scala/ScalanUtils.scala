package scalan.plugin

trait ScalanUtils { self: ScalanPluginCake =>
  /** Imports scalan._ and other packages needed for Scalan and further transformations. */
  def addImports(module: SEntityModuleDef) = {
    val newImports = List(
      SImportStat("scalan._")
    )
    module.copy(imports = newImports)
  }
}
