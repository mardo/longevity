package longevity.integration.model

import longevity.TestLongevityConfigs
import longevity.model.annotations.domainModel
import scala.concurrent.Future

/** covers a persistent with a primary key that contains multiple properties */
package object primaryKeyWithComplexPartialPartition {

  @domainModel trait DomainModel

  val contexts = TestLongevityConfigs.sparseContextMatrix[Future, DomainModel]()

}
