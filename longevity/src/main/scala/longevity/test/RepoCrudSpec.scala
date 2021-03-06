package longevity.test

import journal.Logger
import longevity.config.BackEnd
import longevity.context.LongevityContext
import longevity.model.PType
import longevity.model.PolyPType
import longevity.model.realized.RealizedKey
import longevity.persistence.Deleted
import longevity.persistence.PState
import longevity.persistence.Repo
import org.scalatest.FlatSpec
import org.scalatest.GivenWhenThen
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.Tag
import scala.util.control.NonFatal
import typekey.TypeKey

/** a [[http://www.scalatest.org/ ScalaTest]] fixture to test a [[longevity.persistence.Repo Repo]].
 * instances of this test are provided in your [[longevity.context.LongevityContext
 * LongevityContext]] via methods `repoCrudSpec` and `inMemRepoCrudSpec`. these methods are added by
 * an implicit conversion from `LongevityContext` to [[longevity.context.TestContext.ScalaTestSpecs
 * ScalaTestSpecs]].
 *
 * the repo CRUD spec exercises create/retrieve/update/delete for all the persistent types in your
 * repo.
 *
 * pardon the nasty ScalaDocs for this class. we haven't figured out how to remove the methods
 * inherited from ScalaTest classes yet.
 * 
 * @tparam F the effect
 * @tparam M the model
 * 
 * @param context the longevity context
 * 
 * @param repo the repo under test. this may be different than the `longevityContext.repo`, as users
 * may want to test against other repo. (for instance, they may want a spec for in-memory repo if
 * other parts of their test suite rely on them.)
 * 
 * @param backEnd the back end we are running against. used to name tests so we can distinguish
 * different back ends in test output
 *
 * @param executionContext the execution context
 */
class RepoCrudSpec[F[_], M] private[longevity] (
  protected val longevityContext: LongevityContext[F, M],
  protected val repo: Repo[F, M],
  private val backEnd: BackEnd)
extends FlatSpec with LongevityIntegrationSpec[F, M] with GivenWhenThen {

  protected val logger = Logger[this.type]

  private val suiteNameSuffix = s"- $backEnd - optimisticLocking: ${longevityContext.config.optimisticLocking}"

  override val suiteName = s"RepoCrudSpec $suiteNameSuffix"

  private val effect = longevityContext.effect

  override def beforeAll = try {
    effect.run(repo.createSchema)
  } catch {
    case NonFatal(e) =>
      logger.error("failed to create schema", e)
      throw e
  }

  override def afterAll = effect.run(repo.closeConnection)

  longevityContext.modelType.pTypePool.values.foreach(new RepoSpec(_))

  private class RepoSpec[P](val pType: PType[M, P]) {
    private implicit val pEv = pType.pEv
    private val realizedPType = longevityContext.modelType.realizedPTypes(pType)
    private val pName = pEv.key.name

    object Create extends Tag("Create")
    object Retrieve extends Tag("Retrieve")
    object Update extends Tag("Update")
    object Delete extends Tag("Delete")

    behavior of s"Repo.create[${pName}] $suiteNameSuffix"

    it should s"persist an unpersisted $pName" taggedAs(Create) in {
      val p = randomP()
      val created: PState[P] = effect.run(repo.create(p))
      created.get should equal (p)

      realizedPType.keySet.foreach { key =>
        val retrieved: PState[P] = retrieveByKey(key, created.get).value
        retrieved.get should equal (p)
      }
    }

    behavior of s"Repo.retrieve[${pName}] $suiteNameSuffix"

    it should s"retrieve a persisted $pName" taggedAs(Retrieve) in {
      val p = randomP()
      val created = effect.run(repo.create(p))

      realizedPType.keySet.foreach { key =>
        val retrieved: PState[P] = retrieveByKey(key, created.get).value
        retrieved.get should equal (p)
      }
    }

    behavior of s"Repo.update[${pName}] $suiteNameSuffix"

    it should s"persist updates to a persisted $pName" taggedAs(Update) in {
      val key = randomPTypeKey
      val originalP = randomP(key)
      val modifiedP = realizedPType.keySet.foldLeft(randomP(key)) { (modified, key) =>
        def updateByOriginalKeyVal[V](key: RealizedKey[M, P, V]) = {
          val originalKeyVal = key.keyValForP(originalP)
          key.updateKeyVal(modified, originalKeyVal)
        }
        updateByOriginalKeyVal(key)
      }

      val created: PState[P] = effect.run(repo.create(originalP))

      val modified: PState[P] = created.modify(e => modifiedP)
      val updated: PState[P] = effect.run(repo.update(modified))

      updated.get should equal (modifiedP)

      realizedPType.keySet.foreach { key =>
        val retrieved: PState[P] = retrieveByKey(key, modifiedP).value
        retrieved.get should equal (modifiedP)
      }
    }

    behavior of s"Repo.delete[${pName}] $suiteNameSuffix"

    it should s"delete a persisted $pName" taggedAs(Delete) in {
      val p = randomP()
      val created: PState[P] = effect.run(repo.create(p))

      val deleted: Deleted[P] = effect.run(repo.delete(created))
      deleted.get should equal (p)

      realizedPType.keySet.foreach { key =>
        val retrieved: Option[PState[P]] = retrieveByKey(key, created.get)
        retrieved.isEmpty should be (true)
      }
    }

    private def randomPTypeKey(): TypeKey[_ <: P] = {
      pType match {
        case polyPType: PolyPType[M, P] =>
          val union = longevityContext.modelType.emblematic.unions(pEv.key)
          val derivedTypeKeys = union.constituentKeys.toSeq
          val randomIndex = math.abs(longevityContext.testDataGenerator.generate[Int]) % derivedTypeKeys.size
          derivedTypeKeys(randomIndex)
        case _ =>
          pEv.key
      }
    }

    private def randomP(key: TypeKey[_ <: P] = pEv.key): P = longevityContext.testDataGenerator.generate(key)

    private def retrieveByKey[V](key: RealizedKey[M, P, V], p: P): Option[PState[P]] = {
      val kv = key.keyValForP(p)
      effect.run(repo.retrieve[P](kv)(pEv, key.key))
    }

  }
 
}
