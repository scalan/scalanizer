package scalan.plugin

import scalan.meta.CodegenConfig

object ScalanPluginConfig {
  /** The folder where the app is located and where the generated code will be stored. */
  val home = "/home/mgekk/scalan/scalanizer-demo"
  /** The flag indicates that generated code (virtualized code, boilerplate and type wrappers)
    * should be stored on the file system. */
  var save: Boolean = true
  /** Reload virtualized code from the file system. */
  var read: Boolean = true
  /** The flag indicates that the plugin has to generate additional information and to store it
    * the debug folder and outputs to the console. */
  var debug: Boolean = false
  /** The flag indicates that Meta AST of entities should be serialized and assigned to some variable
    * inside virtualized code. */
  var saveMetaAst: Boolean = false
  /** Mapping of entities and their concrete classes. */
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
  /** The types that shouldn't be Rep[]. */
  val typeClasses = List("Elem", "Cont", "ClassTag")

  /** Config for Scalan META. */
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
}
