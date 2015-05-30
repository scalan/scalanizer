package scalan.plugin

import java.io.File
import scalan.meta.ScalanAst._
import scalan.util.FileUtil

trait Enricher {
  /** Module parent is replaced by the parent with its extension. */
  def composeParentWithExt(module: SEntityModuleDef) = {
    val parentsWithExts = module.ancestors.map{ancestor =>
      ancestor.copy(name = ancestor.name + "Dsl")
    }

    module.copy(ancestors = parentsWithExts)
  }

  /** Imports scalan._ and other packages needed by Scalan and further transformations. */
  def addImports(module: SEntityModuleDef) = {
    val usedModules = ScalanPluginState.usageMap.getOrElse(module.name, List())
    val usedImports = usedModules.map{moduleName =>
      val pkgOfModule = ScalanPluginState.pkgOfModule.get(moduleName) match {
        case Some(pkgName) => pkgName + "."
        case _ => ""
      }
      SImportStat(pkgOfModule + "implOf"+moduleName+".StagedEvaluation._")
    }

    module.copy(imports = SImportStat("scalan._") :: usedImports)
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

  /** Extends the module by Base from Scalan */
  def addModuleAncestors(module: SEntityModuleDef) = {
    val newAncestors = STraitCall(name = "Base", tpeSExprs = List()) :: module.ancestors

    module.copy(ancestors = newAncestors)
  }

  /** Extends the entiry T by Reifiable[T] */
  def addEntityAncestors(module: SEntityModuleDef) = {
    val newAncestors = STraitCall(
      name = "Reifiable",
      tpeSExprs = List(STraitCall(module.entityOps.name,
                                  module.entityOps.tpeArgs.map(arg => STraitCall(arg.name, List()))))
    ) :: module.entityOps.ancestors
    val newEntity = module.entityOps.copy(ancestors = newAncestors)

    module.copy(entityOps = newEntity, entities = List(newEntity))
  }

  def selfComponentsWithSuffix(module: SEntityModuleDef, suffix: String): List[STpeExpr] = {
    module.selfType match {
      case Some(selfTypeDef) => selfTypeDef.components.map{(c: STpeExpr) => c match {
        case tr: STraitCall => tr.copy(name = c.name + suffix)
        case _ => c
      }}
      case _ => List(STraitCall(module.name + suffix, List()))
    }
  }

  /** Puts the module to the cake. For example, trait Segments is transformed to
    * trait Segments {self: SegmentsDsl => ... } */
  def updateSelf(module: SEntityModuleDef) = {
    module.copy(selfType = Some(SSelfTypeDef(
      name = "self",
      components = selfComponentsWithSuffix(module, "Dsl")
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
      isOverride = false,
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

  def eraseModule(module: SEntityModuleDef) = module

  def firstKindArgs(tpeArgs: List[STpeArg]): List[STpeArg] = {
    tpeArgs.filter(_.tparams.isEmpty)
  }

  def highKindArgs(tpeArgs: List[STpeArg]): List[STpeArg] = {
    tpeArgs.filter(!_.tparams.isEmpty)
  }

  def genEntityImpicits(module: SEntityModuleDef) = {
    def genImplicit(tpeArg: STpeArg, methodPrefix: String, resPrefix: String) =
      SMethodDef(name = methodPrefix + tpeArg.name,
        tpeArgs = Nil, argSections = Nil,
        tpeRes = Some(STraitCall(resPrefix, List(STraitCall(tpeArg.name, Nil)))),
        isImplicit = true, isOverride = false,
        overloadId = None, annotations = Nil,
        body = None, isElemOrCont = true)

    def genElem(tpeArg: STpeArg) = genImplicit(tpeArg, "e", "Elem")
    def genCont(tpeArg: STpeArg) = genImplicit(tpeArg, "c", "Cont")

    def genImplicitDefs(tpeArgs: List[STpeArg]): List[SMethodDef] = {
      tpeArgs.map{tpeArg =>
        if (tpeArg.tparams.isEmpty) genElem(tpeArg)
        else genCont(tpeArg)
      }
    }

    val bodyWithImpElems = genImplicitDefs(module.entityOps.tpeArgs) ++ module.entityOps.body
    val newEntity = module.entityOps.copy(body = bodyWithImpElems)

    module.copy(entityOps = newEntity, entities = List(newEntity))
  }

  def genImplicitClassArgs(module: SEntityModuleDef, clazz: SClassDef): List[SClassArg] = {
    def genImplicit(argPrefix: String, argSuffix: String,
                    typePrefix: String, typeSuffix: String) = {
      SClassArg(impFlag = true,
        overFlag = false, valFlag = false,
        name = argPrefix + argSuffix,
        tpe = STraitCall(typePrefix, List(STraitCall(typeSuffix, Nil))),
        default = None, annotations = Nil, isElemOrCont = true)
    }
    def genElem(valName: String, typeName: String) =
      genImplicit("e", valName, "Elem", typeName)
    def genCont(valName: String, typeName: String) =
      genImplicit("c", valName, "Cont", typeName)
    def genImplicitArg(isFirstKind: Boolean, valName: String, typeName: String): SClassArg = {
      if (isFirstKind) genElem(valName, typeName)
      else genCont(valName, typeName)
    }
    def getEntityByAncestor(ancestor: STraitCall): Option[STraitDef] = {
      module.entities.find(entity => entity.name == ancestor.name)
    }
    lazy val ancestorPairs: List[(STpeExpr, STpeArg)] = {
      val ancestors: List[STraitCall] = clazz.ancestors

      ancestors.flatMap { (ancestor: STraitCall) =>
        val optEntity: Option[STraitDef] = getEntityByAncestor(ancestor)

        optEntity match {
          case Some(entity) => ancestor.tpeSExprs zip entity.tpeArgs
          case None => List[(STpeExpr, STpeArg)]()
        }
      }
    }

    clazz.tpeArgs.map { tpeArg => ancestorPairs.find(pair => tpeArg.name == pair._1.name) match {
      case Some((aParam, eParam)) => aParam match {
        case _: STraitCall => genImplicitArg(eParam.tparams.isEmpty, eParam.name, aParam.name)
        case _ => throw new NotImplementedError(s"genImplicitClassArgs: $eParam")
      }
      case None => genImplicitArg(tpeArg.tparams.isEmpty, tpeArg.name, tpeArg.name)
    }}
  }

  def genClassesImplicits(module: SEntityModuleDef) = {
    val newClasses = module.concreteSClasses.map{clazz =>
      val elemArgs = genImplicitClassArgs(module, clazz)
      val newImplicitArgs = SClassArgs(clazz.implicitArgs.args ++ elemArgs)

      clazz.copy(implicitArgs = newImplicitArgs)
    }

    module.copy(concreteSClasses = newClasses)
  }

  def genImplicitMethodArgs(method: SMethodDef): SMethodDef = {
    def genImplicit(tpeArg: STpeArg, valPrefix: String, resPrefix: String) = {
      SMethodArg(impFlag = true, overFlag = false,
        name = valPrefix + tpeArg.name,
        tpe = STraitCall(resPrefix, List(STraitCall(tpeArg.name, Nil))),
        default = None, annotations = Nil, isElemOrCont = true)
    }
    def genElem(tpeArg: STpeArg) = genImplicit(tpeArg, "e", "Elem")
    def genCont(tpeArg: STpeArg) = genImplicit(tpeArg, "c", "Cont")

    def genImplicitVals(tpeArgs: List[STpeArg]): List[SMethodArg] = {
      tpeArgs.map{tpeArg =>
        if (tpeArg.tparams.isEmpty) genElem(tpeArg)
        else genCont(tpeArg)
      }
    }
    val args = method.argSections ++ List(SMethodArgs(genImplicitVals(method.tpeArgs)))

    method.copy(argSections = args)
  }

  def genMethodsImplicits(module: SEntityModuleDef) = {
    def genBodyItem(item: SBodyItem): SBodyItem = item match {
      case m: SMethodDef => genImplicitMethodArgs(m)
      case _ => item
    }
    def genEntity(entity: STraitDef): STraitDef = {
      val newBodyItems = entity.body.map(genBodyItem)
      entity.copy(body = newBodyItems)
    }
    def genEntities(entities: List[STraitDef]): List[STraitDef] = {
      entities.map(genEntity)
    }
    def genClass(clazz: SClassDef): SClassDef = {
      val newBodyItems = clazz.body.map(genBodyItem)
      clazz.copy(body = newBodyItems)
    }
    def genClasses(classes: List[SClassDef]): List[SClassDef] = {
      classes.map(genClass)
    }

    module.copy(entityOps = genEntity(module.entityOps),
      entities = genEntities(module.entities),
      concreteSClasses = genClasses(module.concreteSClasses)
    )
  }

  def genExtensions(module: SEntityModuleDef): List[STraitDef] = {
    val boilerplateSuffix = Map("Dsl" -> "Abs", "DslSeq" -> "Seq", "DslExp" -> "Exp")
    val extensions = ScalanPluginState.extMap(module.name)

    (extensions map {extName =>
      val extSuffix = extName.stripPrefix(module.name)
      val selfType: SSelfTypeDef = SSelfTypeDef(
        name = "self",
        components = selfComponentsWithSuffix(module, extSuffix)
      )
      val boilerplate = STraitCall(module.name + boilerplateSuffix(extSuffix), Nil)
      val ancestors: List[STraitCall] = module.ancestors map {
        ancestor => ancestor.copy(name = ancestor.name + extSuffix)
      }

      STraitDef(
        name = extName,
        tpeArgs = Nil,
        ancestors = boilerplate :: ancestors,
        body = Nil,
        selfType = Some(selfType),
        companion = None,
        annotations = Nil
      )
    }).toList
  }
}
