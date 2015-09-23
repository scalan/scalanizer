package scalan.plugin

import scalan.meta.CodegenConfig

object ScalanPluginConfig {
  var save: Boolean = true
  var read: Boolean = true
  var debug: Boolean = false
  var saveMetaAst: Boolean = false
  val codegenConfig = CodegenConfig(
    name = "Scalan Plugin",
    srcPath = "/",
    entityFiles = List[String](
      "Nums.scala"
      ,"NumMonoids.scala"
      ,"Cols.scala"
      ,"Vecs.scala"
      ,"Matrs.scala"
      ,"MatrOps.scala"
      ,"LinearAlgebraOps.scala"
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
  val typeClasses = List[String]("Elem", "Cont", "ClassTag")
  val home = "/home/mgekk/scalan/scalanizer-demo"
  val entities = Map[String, Set[String]](
    "LinearAlgebra" -> Set(),
    "Num" -> Set("DoubleNum"),
    "NumMonoid" -> Set("PlusMonoid"),
    "Col" -> Set("ColOverArray", "PairCol"),
    "Vec" -> Set("DenseVec"),
    "Matr" -> Set("DenseMatr"),
    "MatrOp" -> Set("BaseMatrOp"),
    "LinearAlgebraOp" -> Set("LA")
  )
}

