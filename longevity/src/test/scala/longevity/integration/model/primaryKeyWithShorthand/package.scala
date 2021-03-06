package longevity.integration.model

import longevity.TestLongevityConfigs
import longevity.model.annotations.domainModel
import scala.concurrent.Future

/** covers a persistent with a key that contains a shorthand */
package object primaryKeyWithShorthand {

  @domainModel trait DomainModel

  val contexts = TestLongevityConfigs.sparseContextMatrix[Future, DomainModel]()

}
