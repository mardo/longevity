package longevity.persistence.mongo

import com.github.nscala_time.time.Imports._
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBObjectBuilder
import emblem.emblematic.Emblem
import emblem.emblematic.EmblemProp
import emblem.emblematic.Emblematic
import emblem.TypeKey
import emblem.emblematic.Union
import emblem.exceptions.CouldNotTraverseException
import emblem.emblematic.traversors.sync.Traversor
import emblem.typeKey
import longevity.exceptions.persistence.NotInSubdomainTranslationException
import scala.reflect.runtime.universe.typeOf

/** translates [[Persistent persistents]] into
 * [[http://mongodb.github.io/casbah/api/#com.mongodb.casbah.commons.MongoDBObject
 * casbah MongoDBObjects]].
 * 
 * embeddables and key values with a single property will be inlined in the BSON.
 *
 * @param emblematic the emblematic types to use
 */
private[persistence] class PersistentToCasbahTranslator(
  private val emblematic: Emblematic) {

  /** translates a Scala object into casbah */
  def translate[A : TypeKey](a: A, isUnionOrTopLevel: Boolean): Any = try {
    traversor.traverse[A](WrappedInput(a, isUnionOrTopLevel))
  } catch {
    case e: CouldNotTraverseException =>
      throw new NotInSubdomainTranslationException(e.typeKey.name, e)
  }

  private val optionAnyType = typeOf[scala.Option[_]]

  case class WrappedInput[A](value: A, isUnionOrTopLevel: Boolean)

  private val traversor = new Traversor {

    type TraverseInput[A] = WrappedInput[A]
    type TraverseResult[A] = Any

    override protected val emblematic = PersistentToCasbahTranslator.this.emblematic

    override protected def traverseBoolean(input: WrappedInput[Boolean]): Any = input.value

    override protected def traverseChar(input: WrappedInput[Char]): Any = input.value.toString

    override protected def traverseDateTime(input: WrappedInput[DateTime]): Any = input.value

    override protected def traverseDouble(input: WrappedInput[Double]): Any = input.value

    override protected def traverseFloat(input: WrappedInput[Float]): Any = input.value

    override protected def traverseInt(input: WrappedInput[Int]): Any = input.value

    override protected def traverseLong(input: WrappedInput[Long]): Any = input.value

    override protected def traverseString(input: WrappedInput[String]): Any = input.value

    override protected def constituentTypeKey[A : TypeKey](
      union: Union[A],
      input: WrappedInput[A])
    : TypeKey[_ <: A] =
      union.typeKeyForInstance(input.value).get

    override protected def stageUnion[A : TypeKey, B <: A : TypeKey](
      union: Union[A],
      input: WrappedInput[A])
    : Iterable[WrappedInput[B]] =
      Seq(input.asInstanceOf[WrappedInput[B]].copy(isUnionOrTopLevel = true))

    override protected def unstageUnion[A : TypeKey, B <: A : TypeKey](
      union: Union[A],
      input: WrappedInput[A],
      result: Iterable[Any])
    : Any =
      result.head.asInstanceOf[DBObject] + ("_discriminator" -> typeKey[B].name)

    override protected def stageEmblemProps[A : TypeKey](
      emblem: Emblem[A],
      input: WrappedInput[A])
    : Iterable[PropInput[A, _]] = {
      def propInput[B](prop: EmblemProp[A, B]) = prop -> WrappedInput(prop.get(input.value), false)
      emblem.props.map(propInput(_))
    }

    override protected def unstageEmblemProps[A : TypeKey](
      emblem: Emblem[A],
      input: WrappedInput[A],
      result: Iterable[PropResult[A, _]])
    : TraverseResult[A] = {
      if (emblem.props.size == 1 && !input.isUnionOrTopLevel) {
        result.head._2
      } else {
        val builder = new MongoDBObjectBuilder()
        result.foreach {
          case (prop, propResult) =>
            if (!(prop.typeKey <:< optionAnyType) || propResult != None) {
              builder += prop.name -> propResult
            }
        }
        builder.result()
      }
    }

    override protected def stageOptionValue[A : TypeKey](
      input: WrappedInput[Option[A]])
    : Iterable[WrappedInput[A]] =
      input.value.toIterable.map(WrappedInput(_, false))

    override protected def unstageOptionValue[A : TypeKey](
      input: WrappedInput[Option[A]],
      result: Iterable[TraverseResult[A]])
    : TraverseResult[Option[A]] =
      result.headOption

    override protected def stageSetElements[A : TypeKey](
      input: WrappedInput[Set[A]])
    : Iterable[WrappedInput[A]] =
      input.value.map(WrappedInput(_, false))

    override protected def unstageSetElements[A : TypeKey](
      input: WrappedInput[Set[A]],
      result: Iterable[TraverseResult[A]])
    : TraverseResult[Set[A]] =
      result.toSet

    override protected def stageListElements[A : TypeKey](
      input: WrappedInput[List[A]])
    : Iterable[WrappedInput[A]] =
      input.value.map(WrappedInput(_, false))

    override protected def unstageListElements[A : TypeKey](
      input: WrappedInput[List[A]],
      result: Iterable[TraverseResult[A]])
    : TraverseResult[List[A]] =
      result.toList

  }

}