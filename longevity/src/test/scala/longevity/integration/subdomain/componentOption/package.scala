package longevity.integration.subdomain

import longevity.TestLongevityConfigs
import longevity.subdomain.annotations.subdomain

/** covers a persistent with an optional component entity */
package object componentOption {

  @subdomain object subdomain

  val contexts = TestLongevityConfigs.sparseContextMatrix(subdomain)

}
