package longevity.model.annotations

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

/** macro annotation to mark a class as a derived persistent component. creates a
 * companion object for the class that extends [[longevity.model.DerivedPType
 * DerivedPType]]. if the class already has a companion object, then adds a parent
 * class `DerivedPType` to the existing companion object. Note that
 * this will not work if your companion object already extends an abstract
 * or concrete class, as `DerivedPType` itself is an abstract class. if this
 * happens, you will see a compiler error such as "class Foo needs to be a trait
 * to be mixed in".
 *
 * @tparam M the model
 *
 * @tparam Poly the type of the polymorphic persistent that this persistent is
 * derived from
 */
@compileTimeOnly("you must enable macro paradise for @derivedPersistent to work")
class derivedPersistent[M, Poly] extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro derivedPersistent.impl

}

private object derivedPersistent {

  def impl(c0: Context)(annottees: c0.Tree*): c0.Tree = new DerivedPersistentImpl {
    val c: c0.type = c0
    val as = annottees
  } .impl

  private abstract class DerivedPersistentImpl extends AbstractPersistentImpl {
    import c.universe._

    protected def name = name0

    private lazy val name0 = as.head match {
      case q"$_ class $typeName[..$_] $_(...$_) extends {..$_} with ..$_ { $_ => ..$_ }" => typeName
      case _ =>
        c.abort(
          c.enclosingPosition,
          s"@longevity.model.annotations.derivedPersistent can only be applied to classes")
    }

    protected def ptype = tq"longevity.model.DerivedPType[$mtype, $typeName, $polyTypeName]"

    protected lazy val (mtype, polyTypeName) = c.prefix.tree match {
      case q"new $_[$mtype, $poly]" => (mtype, poly)
      case _ =>
        c.abort(
          c.enclosingPosition,
          s"@longevity.model.annotations.derivedPersistent must take exactly two type arguments")
    }

  }

}
