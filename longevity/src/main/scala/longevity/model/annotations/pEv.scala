package longevity.model.annotations

import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/** macro annotation to generate a concrete definition of `PType.pEv` for
 * you. This is roughly built as follows:
 *
 * {{{
 *  implicit val pEv: longevity.model.PEv[M, P] = {
 *    import org.scalacheck.ScalacheckShapeless._
 *    implicit val arbJoda = com.fortysevendeg.scalacheck.datetime.joda.ArbitraryJoda.arbJoda
 *    new longevity.model.PEv[$m, $p]
 *  }
 * }}}
 */
@compileTimeOnly("you must enable macro paradise for @pEv to work")
class pEv extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro pEv.impl

}

private object pEv {

  def impl(c0: Context)(annottees: c0.Tree*): c0.Tree = new MPropsImpl {
    val c: c0.type = c0
    val as = annottees
  } .impl

  private abstract class MPropsImpl {
    val c: Context
    val as: Seq[c.Tree]

    import c.universe._

    def impl = if (as.tail.isEmpty) expanded else q"{ $expanded ; ${as.tail.head} }"

    private def expanded: c.Tree = {
      def ev(ps: Seq[c.Tree]) = defPEv(ps.head)
      as.head match {
        case q"$ms object $n                       extends {..$eds} with ..$ps { $s =>             ..$ss }" =>
             q"$ms object $n                       extends {..$eds} with ..$ps { $s => ${ev(ps)} ; ..$ss }"
        case q"$ms class  $n[..$tps] $cms(...$pss) extends {..$eds} with ..$ps { $s =>             ..$ss }" =>
             q"$ms class  $n[..$tps] $cms(...$pss) extends {..$eds} with ..$ps { $s => ${ev(ps)} ; ..$ss }"
        case q"$ms trait  $n[..$tps]               extends {..$eds} with ..$ps { $s =>             ..$ss }" =>
             q"$ms trait  $n[..$tps]               extends {..$eds} with ..$ps { $s => ${ev(ps)} ; ..$ss }"
        case _ => misapplied()
      }
    }

    private def misapplied() =
      c.abort(
        c.enclosingPosition,
        s"@longevity.model.annotations.pEv can only be applied to a longevity.model.PType")

    private def defPEv(parent: c.Tree) = {
      c.typecheck(parent, c.TYPEmode, withMacrosDisabled = false) match {
        case tq"longevity.model.PType[$m, $p]"            => pEvForMP(m.tpe, p.tpe)
        case tq"longevity.model.PolyPType[$m, $p]"        => pEvForMP(m.tpe, p.tpe)
        case tq"longevity.model.DerivedPType[$m, $p, $q]" => pEvForMP(m.tpe, p.tpe)
        case _                                            => misapplied()
      }
    }

    private def pEvForMP(m: c.Type, p: c.Type) = q"""
      implicit val pEv: longevity.model.PEv[$m, $p] = {
        import org.scalacheck.ScalacheckShapeless._
        implicit val arbJoda = com.fortysevendeg.scalacheck.datetime.joda.ArbitraryJoda.arbJoda
        new longevity.model.PEv[$m, $p]
      }
    """
  }

}
