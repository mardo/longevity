package longevity.repo

import scala.reflect.runtime.universe.TypeTag

import emblem._
import longevity.domain._

object testUtil {

  class DummyRepo[E <: Entity : TypeTag](
    override val entityType: EntityType[E]
  )(
    implicit repoPool: RepoPool
  ) extends Repo[E](repoPool) {
    def create(e: Unpersisted[E]) = ???
    def retrieve(id: Id[E]) = ???
    def update(p: Persisted[E]) = ???
    def delete(p: Persisted[E]) = ???
  }

  case class User(name: String) extends Entity

  object UserType extends EntityType[User]

  case class Post(author: Assoc[User], content: String) extends Entity

  object PostType extends EntityType[Post]

}
