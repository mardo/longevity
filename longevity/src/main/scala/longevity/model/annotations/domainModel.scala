package longevity.model.annotations

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

/** macro annotation to mark an object as a domain model. extends the class with
 * `longevity.model.DomainModel(currentPackage)`, where `currentPackage` is
 * the name of the package in which this annotation was applied.
 */
@compileTimeOnly("you must enable macro paradise for @domainModel to work")
class domainModel[P] extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro domainModel.impl

}

private object domainModel {

  def impl(c0: Context)(annottees: c0.Tree*): c0.Tree = new DomainModelImpl {
    val c: c0.type = c0
    val as = annottees
  } .impl

  private abstract class DomainModelImpl {
    val c: Context
    val as: Seq[c.Tree]

    import c.universe._

    def impl = {
      as.head match {
        case q"$ms object $n extends {..$eds} with ..$ps          { $s => ..$ss }" =>
             q"$ms object $n extends {..$eds} with ..${newPs(ps)} { $s => ..$ss }"
        case _ =>
          c.abort(c.enclosingPosition, s"@longevity.model.annotations.domainModel can only be applied to objects")
      }
    }

    def newPs(ps: Seq[c.Tree]) = {
      def owningPackage(s: c.Symbol): c.Symbol = if (s.isPackage) s else owningPackage(s.owner)
      val p = owningPackage(c.internal.enclosingOwner)
      val model =
        Apply(
          Select(Select(Ident(TermName("longevity")), TermName("model")),
                 TypeName("DomainModel")),
          List(Literal(Constant(p.fullName))))
      model +: ps.tail
    }

  }

}
