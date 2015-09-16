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
    isAlreadyRep = false,
    isSeqEnabled = false
  )
  val typeClasses = List[String]("Default")
  val home = "/home/mgekk/scalan/scalanizer-demo"
  val externalTypes = Set[String](
    "CanBuildFrom",
    "Array"
//    "Num", "DoubleNum",
//    "NumMonoid", "PlusMonoid",
//    "Col", "ColOverArray", "PairCol",
//    "Vec", "DenseVec",
//    "Matr", "DenseMatr",
//    "MatrOp", "BaseMatrOp"
  )
}

