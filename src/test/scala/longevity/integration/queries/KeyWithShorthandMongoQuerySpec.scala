package longevity.integration.queries

import longevity.test.QuerySpec
import longevity.integration.subdomain.keyWithShorthand._
import scala.concurrent.ExecutionContext.Implicits.global

class KeyWithShorthandMongoQuerySpec
extends QuerySpec[KeyWithShorthand](mongoContext, mongoContext.testRepoPool) {

  lazy val sample = randomP

  val secondaryKeyProp = KeyWithShorthand.prop[SecondaryKey]("secondaryKey")

  import KeyWithShorthand.queryDsl._

  behavior of "MongoRepo.retrieveByQuery"

  it should "produce expected results for simple equality queries" in {
    exerciseQuery(secondaryKeyProp eqs sample.secondaryKey)
    exerciseQuery(secondaryKeyProp neq sample.secondaryKey)
  }

  it should "produce expected results for simple ordering queries" in {
    exerciseQuery(secondaryKeyProp gt sample.secondaryKey)
    exerciseQuery(secondaryKeyProp gte sample.secondaryKey)
    exerciseQuery(secondaryKeyProp lt sample.secondaryKey)
    exerciseQuery(secondaryKeyProp lte sample.secondaryKey)
  }

}
