package scalan.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform

/** The class adds annotations to objects. */
class AddAnnot(val global: Global) extends PluginComponent with Transform  {
  import global._

  val phaseName: String = "scalan-annotation"

  val runsAfter = List[String]("scalan-import")
  override val runsRightAfter: Option[String] = Some("scalan-import")

  def newTransformer(unit: CompilationUnit) = new AnnotTransformer

  class AnnotTransformer extends Transformer {
    def addAnnotation(mods: Modifiers, name: String): Modifiers = {
      val annotName: Name = newTypeName(name)
      val annotContr: Name = newTermName ("<init>")
      val annot = Apply(Select(New(Ident(annotName)), annotContr), List[Tree]())

      Modifiers(mods.flags, mods.privateWithin, mods.annotations ++ List[Tree](annot))
    }

    def addAbstract(mods: Modifiers): Modifiers = {
      Modifiers(mods.flags | scala.reflect.internal.ModifierFlags.ABSTRACT, mods.privateWithin, mods.annotations)
    }

    def annotateCakeSlice(stats: List[Tree]): List[Tree] = {
      stats.map{(stat: Tree) => stat match {
        case q"""$mods trait Segm[..$tparams]
                    extends { ..$earlydefns } with ..$parents
                    { $self => ..$stats }
                 """ =>
          val newmods = addAnnotation(mods, "CommonUDT")
          //print("Adding of @CommonUDT to Segm")
          q"""$newmods trait Segm[..$tparams]
                 extends { ..$earlydefns } with ..$parents
                 { $self => ..$stats }
             """
        case q"""$mods class Interval[..$tparams] $ctorMods(...$paramss)
                    extends { ..$earlydefns } with ..$parents
                    { $self => ..$stats }
                 """ =>
          val newmods = addAnnotation(addAbstract(mods), "DefaultUDT")
          //print("Adding of @DefaultUDT to Interval")
          q"""$newmods class Interval[..$tparams] $ctorMods(...$paramss)
                extends { ..$earlydefns } with ..$parents
                { $self => ..$stats }
             """
        case q"""$mods class $tpname[..$tparams] $ctorMods(...$paramss)
                    extends { ..$earlydefns } with ..$parents
                    { $self => ..$stats }
                 """ =>
          //print("Adding of @UDT to " + tpname)
          val newmods = addAnnotation(addAbstract(mods), "UDT")
          q"""$newmods class $tpname[..$tparams] $ctorMods(...$paramss)
                extends { ..$earlydefns } with ..$parents
                { $self => ..$stats }
             """
        case _ => stat
      }}
    }

    override def transform(tree: Tree): Tree = tree match {
      case PackageDef(segs @ Ident(TermName("segms")), pkgstats: List[Tree]) =>
        val newstats = pkgstats.map(stat => stat match {
          case q"""$mods trait Segms[..$tparams]
                    extends { ..$earlydefns } with ..$parents
                    { $self => ..$stats }
                 """ =>
            val newmods = addAnnotation(mods, "CakeSlice")
            val newstats = annotateCakeSlice(stats)
            //print("Adding of @CakeSlice to Segms")
            q"""$newmods trait Segms[..$tparams]
                 extends { ..$earlydefns } with ..$parents
                 { $self => ..$newstats }
             """
          case _ => stat
        })

        PackageDef(segs, newstats)
      case _ => tree
    }
  }
}
