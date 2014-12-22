package longevity.domain

import scala.language.implicitConversions

object Assoc {

  /** wraps an entity in a UnpersistedAssoc when needed */
  implicit def apply[E <: Entity](e: E): Assoc[E] = UnpersistedAssoc(e)

  class AssocIsUnpersistedException[E <: Entity](val assoc: Assoc[E])
  extends Exception("cannot retrieve from an unpersisted assoc")

  class AssocIsPersistedException[E <: Entity](val assoc: Assoc[E])
  extends Exception("cannot get an unpersisted entity from a persisted assoc")

}

/** a unidirectional association between two domain entities. the left side of the association - that is,
  * the holder of the `Assoc` instance - is known as the associator. the right side of the association is
  * the associatee.
  *
  * 
  */
trait Assoc[E <: Entity] {

  /** prevent subtyping outside of longevity library */
  private[longevity] val _lock: Int

  /** true whenever the assoc is with a persisted entity */
  def isPersisted: Boolean

  /** retrieves the persisted associatee from the assoc */
  @throws[Assoc.AssocIsUnpersistedException[E]]("whenever the assoc is not persisted")
  def retrieve: E

  /** retrieves an unpersisted associatee from the assoc */
  @throws[Assoc.AssocIsPersistedException[E]]("whenever the assoc is persisted")
  def unpersisted: E

  /** gets the underlying assoc, whether persisted or not */
  def get: E
}

case class UnpersistedAssoc[E <: Entity](unpersisted: E) extends Assoc[E] {
  private[longevity] val _lock = 0
  def isPersisted = false
  def retrieve = throw new Assoc.AssocIsUnpersistedException(this)
  def get = unpersisted
}

