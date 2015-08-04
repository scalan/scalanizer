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

  def getImportByName(name: String): SImportStat = {
    val pkgOfModule = ScalanPluginState.pkgOfModule.get(name) match {
      case Some(pkgName) => pkgName + "."
      case _ => ""
    }
    SImportStat(pkgOfModule + "implOf"+name+".StagedEvaluation._")
  }

  /** Imports scalan._ and other packages needed by Scalan and further transformations. */
  def addImports(module: SEntityModuleDef) = {
    val usedModules = ScalanPluginState.usageMap.getOrElse(module.name, List())
    val usedImports = usedModules.map(getImportByName)
    val usersImport = module.imports.collect{
      case imp @ SImportStat("scalan.compilation.KernelTypes._") => imp
    }

    module.copy(imports = SImportStat("scalan._") :: (usedImports ++ usersImport))
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

  def selfComponentsWithSuffix(moduleName: String,
                               selfType: Option[SSelfTypeDef],
                               suffix: String): List[STpeExpr] = {
    selfType match {
      case Some(selfTypeDef) => selfTypeDef.components.map{(c: STpeExpr) => c match {
        case tr: STraitCall => tr.copy(name = c.name + suffix)
        case _ => c
      }}
      case _ => List(STraitCall(moduleName + suffix, List()))
    }
  }

  def selfModuleComponents(module: SEntityModuleDef, suffix: String): List[STpeExpr] = {
    selfComponentsWithSuffix(module.name, module.selfType, suffix)
  }

  /** Puts the module to the cake. For example, trait Segments is transformed to
    * trait Segments {self: SegmentsDsl => ... } */
  def updateSelf(module: SEntityModuleDef) = {
    module.copy(selfType = Some(SSelfTypeDef(
      name = "self",
      components = selfModuleComponents(module, "Dsl")
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
  
  def convertCompanion(comp: STraitOrClassDef): STraitOrClassDef = comp match {
    case obj: SObjectDef =>
      STraitDef(name = obj.name + "Companion",
        tpeArgs = obj.tpeArgs, ancestors = obj.ancestors, body = obj.body, selfType = obj.selfType,
        companion = obj.companion, annotations = obj.annotations)
    case _ => comp
  }

  /** Checks that the entity has a companion. If the entity doesn't have it
    * then the method adds the companion. */
  def checkEntityCompanion(module: SEntityModuleDef) = {
    val entity = module.entityOps
    val newCompanion = entity.companion match {
      case Some(comp) => Some(convertCompanion(comp))
      case None => Some(createCompanion(entity.name))
    }
    val newEntity = entity.copy(companion = newCompanion)

    module.copy(entityOps = newEntity, entities = List(newEntity))
  }

  /** Checks that concrete classes have their companions. */
  def checkClassCompanion(module: SEntityModuleDef) = {
    val newClasses = module.concreteSClasses.map{ clazz =>
      val newCompanion = clazz.companion match {
        case Some(comp) => Some(convertCompanion(comp))
        case None => Some(createCompanion(clazz.name))
      }

      clazz.copy(companion = newCompanion)
    }

    module.copy(concreteSClasses = newClasses)
  }

  def saveDebugCode(fileName: String, code: String) = {
    val folder = new File(ScalanPluginConfig.home)
    val file = FileUtil.file(folder, "debug", fileName)
    file.mkdirs()

    FileUtil.write(file, code)
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

  def genElemsByTypeArgs(tpeArgs: List[STpeArg]): List[SMethodDef] = {
    def genImplicit(tpeArg: STpeArg, methodPrefix: String, resPrefix: String) =
      SMethodDef(name = methodPrefix + tpeArg.name,
        tpeArgs = Nil, argSections = Nil,
        tpeRes = Some(STraitCall(resPrefix, List(STraitCall(tpeArg.name, Nil)))),
        isImplicit = true, isOverride = false,
        overloadId = None, annotations = Nil,
        body = None, isElemOrCont = true)

    def genElem(tpeArg: STpeArg) = genImplicit(tpeArg, "ee", "Elem")
    def genCont(tpeArg: STpeArg) = genImplicit(tpeArg, "ce", "Cont")

    tpeArgs.map{tpeArg =>
      if (tpeArg.tparams.isEmpty) genElem(tpeArg)
      else genCont(tpeArg)
    }
  }

  def genEntityImpicits(module: SEntityModuleDef) = {

    val bodyWithImpElems = genElemsByTypeArgs(module.entityOps.tpeArgs) ++ module.entityOps.body
    val newEntity = module.entityOps.copy(body = bodyWithImpElems)

    module.copy(entityOps = newEntity, entities = List(newEntity))
  }

  def genImplicitClassArgs(module: SEntityModuleDef, clazz: SClassDef): List[SClassArg] = {
    def genImplicit(argPrefix: String, argSuffix: String,
                    typePrefix: String, typeSuffix: STpeExpr) = {
      SClassArg(impFlag = true,
        overFlag = false, valFlag = false,
        name = argPrefix + argSuffix,
        tpe = STraitCall(typePrefix, List(typeSuffix)),
        default = None, annotations = Nil, isElemOrCont = true)
    }
    def genElem(valName: String, typeName: STpeExpr) =
      genImplicit("e", valName, "Elem", typeName)
    def genCont(valName: String, typeName: STpeExpr) =
      genImplicit("c", valName, "Cont", typeName)
    def genImplicitArg(isFirstKind: Boolean, valName: String, typeName: STpeExpr): SClassArg = {
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
    def tpeArg2Expr(tpeArg: STpeArg): STpeExpr = STraitCall(tpeArg.name, Nil)
    val classImplicits = clazz.tpeArgs.map { tpeArg =>
      ancestorPairs.find(pair => tpeArg2Expr(tpeArg) == pair._1) match {
        case Some((aParam, eParam)) => aParam match {
          case _: STraitCall => genImplicitArg(eParam.tparams.isEmpty, "e"+eParam.name, aParam)
          case _ => throw new NotImplementedError(s"genImplicitClassArgs: $eParam")
        }
        case None => genImplicitArg(tpeArg.tparams.isEmpty, "c"+tpeArg.name, tpeArg2Expr(tpeArg))
    }}
    val entityImplicits = ancestorPairs.map{pair =>
      val (ancestorParam, entityParam) = pair
      genImplicitArg(entityParam.tparams.isEmpty, "e"+entityParam.name, ancestorParam)
    }

    (entityImplicits ++ classImplicits).distinct
  }

  def genClassesImplicits(module: SEntityModuleDef) = {
    def unpackElem(classArg: SClassArg): Option[STpeExpr] = classArg.tpe match {
      case STraitCall("Elem", List(prim @ STpePrimitive(_,_))) => Some(prim)
      case _ => None
    }
    /** The function checks that the Elem is already defined somewhere in scope. */
    def isElemAlreadyDefined(classArg: SClassArg): Boolean = unpackElem(classArg) match {
      case Some(_) => true
      case None => false
    }
    def convertElemValToMethod(classArg: SClassArg): SMethodDef = {
      SMethodDef(name = classArg.name, tpeArgs = Nil, argSections = Nil,
        tpeRes = Some(classArg.tpe),
        isImplicit = false, isOverride = false, overloadId = None, annotations = Nil,
        body = Some(STypeApply(SIdent("element"), unpackElem(classArg).toList)),
        isElemOrCont = true)
    }
    val newClasses = module.concreteSClasses.map{clazz =>
      val (definedElems, elemArgs) = genImplicitClassArgs(module, clazz) partition isElemAlreadyDefined
      val newImplicitArgs = SClassArgs(clazz.implicitArgs.args ++ elemArgs)
      val newBody = definedElems.map(convertElemValToMethod) ++ clazz.body

      clazz.copy(implicitArgs = newImplicitArgs, body = newBody)
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
    def genElem(tpeArg: STpeArg) = genImplicit(tpeArg, "em", "Elem")
    def genCont(tpeArg: STpeArg) = genImplicit(tpeArg, "cm", "Cont")
    def genImplicitVals(tpeArgs: List[STpeArg]): List[SMethodArg] = {
      tpeArgs.map{tpeArg =>
        if (tpeArg.tparams.isEmpty) genElem(tpeArg)
        else genCont(tpeArg)
      }
    }
    val args = genImplicitVals(method.tpeArgs) match {
      case Nil => method.argSections
      case as => method.argSections ++ List(SMethodArgs(as))
    }

    method.copy(argSections = joinImplicitArgs(args))
  }

  def genMethodsImplicits(module: SEntityModuleDef) = {
    def genBodyItem(item: SBodyItem): SBodyItem = item match {
      case m: SMethodDef => genImplicitMethodArgs(m)
      case _ => item
    }
    def genCompanion(companion: Option[STraitOrClassDef]) = companion match {
      case Some(t : STraitDef) => Some(t.copy(body = t.body.map(genBodyItem)))
      case Some(c : SClassDef) => Some(c.copy(body = c.body.map(genBodyItem)))
      case Some(unsupported) => throw new NotImplementedError(s"genCompanion: $unsupported")
      case None => None
    }
    def genEntity(entity: STraitDef): STraitDef = {
      val newBodyItems = entity.body.map(genBodyItem)
      entity.copy(body = newBodyItems, companion = genCompanion(entity.companion))
    }
    def genEntities(entities: List[STraitDef]): List[STraitDef] = {
      entities.map(genEntity)
    }
    def genClass(clazz: SClassDef): SClassDef = {
      val newBodyItems = clazz.body.map(genBodyItem)
      clazz.copy(body = newBodyItems, companion = genCompanion(clazz.companion))
    }
    def genClasses(classes: List[SClassDef]): List[SClassDef] = {
      classes.map(genClass)
    }

    module.copy(entityOps = genEntity(module.entityOps),
      entities = genEntities(module.entities),
      concreteSClasses = genClasses(module.concreteSClasses)
    )
  }

  def genExtensions(moduleName: String,
                    selfModuleType: Option[SSelfTypeDef],
                    moduleAncestors: List[STraitCall]
                    ): List[STraitDef] = {
    val boilerplateSuffix = Map("Dsl" -> "Abs", "DslSeq" -> "Seq", "DslExp" -> "Exp")
    val extensions = ScalanPluginState.extMap(moduleName)

    (extensions map {extName =>
      val extSuffix = extName.stripPrefix(moduleName)
      val selfType: SSelfTypeDef = SSelfTypeDef(
        name = "self",
        components = selfComponentsWithSuffix(moduleName, selfModuleType, extSuffix)
      )
      val boilerplate = STraitCall(moduleName + boilerplateSuffix(extSuffix), Nil)
      val ancestors: List[STraitCall] = moduleAncestors map {
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

  def genModuleExtensions(module: SEntityModuleDef): List[STraitDef] = {
    genExtensions(module.name, module.selfType, module.ancestors)
  }

  /** ClassTags are removed because they can be extracted from Elems. */
  def filterClassTags(module: SEntityModuleDef) = {
    def filterClassTagInClassArgs(classArgs: SClassArgs) = {
      val args = classArgs.args.filter{carg => carg.tpe match {
        case tc: STraitCall if tc.name == "ClassTag" => false
        case _ => true
      }}
      classArgs.copy(args = args)
    }
    def filerClassTagInMethodArgs(methodArgs: SMethodArgs) = {
      val args = methodArgs.args.filter{marg => marg.tpe match {
        case tc: STraitCall if tc.name == "ClassTag" => false
        case _ => true
      }}
      methodArgs.copy(args = args)
    }
    def filterClassTagInMethod(m: SMethodDef): SMethodDef = {
      val argSections = m.argSections.map(filerClassTagInMethodArgs)
      val body = m.body.map(_ match {
        case methodDef: SMethodDef => filterClassTagInMethod(methodDef)
        case item => item
      })

      m.copy(argSections = argSections, body = body)
    }
    def filterClassTagInBody(blist: List[SBodyItem]) = {
      blist.map{_ match {
        case m: SMethodDef => filterClassTagInMethod(m)
        case item => item
      }}.filter{_ match {
        case SMethodDef(_,_,_,Some(STraitCall("ClassTag", _)),true,_,_,_,_,_) => false
        case _ => true
      }}
    }
    val entityOps = module.entityOps.copy(body = filterClassTagInBody(module.entityOps.body))
    val entities = module.entities.map{entity => entity.copy(body = filterClassTagInBody(entity.body))}
    val concreteSClasses = module.concreteSClasses.map{clazz =>
      clazz.copy(
        args = filterClassTagInClassArgs(clazz.args),
        implicitArgs = filterClassTagInClassArgs(clazz.implicitArgs),
        body = filterClassTagInBody(clazz.body)
      )
    }

    module.copy(entityOps = entityOps, entities = entities, concreteSClasses = concreteSClasses)
  }
  /** According to scala docs, a method or constructor can have only one implicit parameter list,
    * and it must be the last parameter list given. */
  def joinImplicitArgs(argSections: List[SMethodArgs]): List[SMethodArgs] = {
    val cleanArgs = argSections.map(_.args)
    val (imp, nonImp) = cleanArgs.partition{_ match {
      case (m: SMethodArg) :: _ => m.impFlag
      case _ => false
    }}
    val newArgs = imp.flatten match {
      case Nil => nonImp
      case as => nonImp ++ List(as)
    }

    newArgs.map(args => SMethodArgs(args))
  }

  def externalTypeToWrapper(module: SEntityModuleDef) = {
    /* TODO: Needed generic approach. */
    def convArgs(argSections: List[SMethodArgs]): List[SMethodArgs] = {
      argSections.map{ methodArgs =>
        val args = methodArgs.args.map{_ match {
          case marg @ SMethodArg(_,_,_,STraitCall("MyArr", params),_,_,_) =>
            marg.copy(tpe = STraitCall("MyArrWrapper", params))
          case rest => rest
        }}
        methodArgs.copy(args = args)
      }
    }
    def convBody(body: List[SBodyItem]): List[SBodyItem] = body.map {_ match {
      case smethod @ SMethodDef(_,_,argSections,Some(STraitCall("MyArr", tparams)), _,_,_,_,_,_) =>
        smethod.copy(argSections = convArgs(argSections), tpeRes = Some(STraitCall("MyArrWrapper", tparams)))
      case smethod: SMethodDef => smethod.copy(argSections = convArgs(smethod.argSections))
      case rest => rest
    }}
    val entityBody = convBody(module.entityOps.body)
    val entity = module.entityOps.copy(body = entityBody)
    val classes = module.concreteSClasses.map{ clazz =>
      val classArgs = clazz.args.args.map { _ match {
        case arg @SClassArg(_,_,_,_,STraitCall("MyArr", params),_,_,_) =>
          arg.copy(tpe = STraitCall("MyArrWrapper", params))
        case rest => rest
      }}
      val companion = clazz.companion.map {_ match {
        case obj: SObjectDef => obj.copy(body = convBody(obj.body))
        case tr: STraitDef => tr.copy(body = convBody(tr.body))
        case unknown => throw new NotImplementedError(unknown.toString)
      }}
      clazz.copy(args = SClassArgs(classArgs), companion = companion)
    }

    module.copy(entityOps = entity, entities = List(entity), concreteSClasses = classes)
  }
}
