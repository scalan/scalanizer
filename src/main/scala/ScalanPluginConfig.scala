package scalan.plugin

import scalan.meta.CodegenConfig

object ScalanPluginConfig {
  var save: Boolean = true
  var read: Boolean = true
  var debug: Boolean = false
  val codegenConfig = CodegenConfig(
    name = "Scalan Plugin",
    srcPath = "/",
    entityFiles = List[String](
//       "Numers.scala"
//      ,"NumMonoids.scala"
//      ,"Cols.scala"
//      ,"Vecs.scala"
//      ,"Matrs.scala"
      "LinearAlgebraOps.scala"
    ),
    baseContextTrait = "ScalanDsl",
    seqContextTrait = "ScalanSeq",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
      "scala.reflect.runtime.universe._",
      "scala.reflect._",
      "scalan.common.Default"
    ),
    entityTypeSynonyms = Map[String, String](),
    isAlreadyRep = false
  )
  val typeClasses = List[String](
    "Numer", "DoubleNumer"
    , "NumMonoid", "PlusMonoid"
  )
}

