package longevity.integration.subdomain

import longevity.TestLongevityConfigs
import longevity.subdomain.annotations.subdomain

/** covers a persistent with option attributes for every supported basic type */
package object basicOptions {

  @subdomain object subdomain

  val contexts = TestLongevityConfigs.sparseContextMatrix(subdomain)

  // nice test-only drop-in for debugging:
  // val contexts = {
  //   import longevity.context.LongevityContext
  //   Seq(new LongevityContext(subdomain, TestLongevityConfigs.mongoConfig))
  // }

}
