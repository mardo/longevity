package longevity.integration.model

import longevity.TestLongevityConfigs
import longevity.model.annotations.domainModel
import scala.concurrent.Future

/** covers a persistent with a key that contains a component */
package object primaryKeyInComponent {

  @domainModel trait DomainModel

  val contexts = TestLongevityConfigs.sparseContextMatrix[Future, DomainModel]()

}
