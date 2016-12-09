package longevity.integration.model

import longevity.TestLongevityConfigs
import longevity.model.annotations.domainModel

package object shorthands {

  @domainModel object domainModel

  val contexts = TestLongevityConfigs.sparseContextMatrix(domainModel)

}
