package longevity.integration.subdomain

import longevity.context.Cassandra
import longevity.context.LongevityContext
import longevity.context.Mongo
import longevity.subdomain.Subdomain
import longevity.subdomain.embeddable.ETypePool
import longevity.subdomain.ptype.PTypePool

/** covers a persistent with an embeddable with an embeddable with a single property  */
package object withComponentWithSinglePropComponent {

  val subdomain = Subdomain(
    "With Component With Single Prop Component",
    PTypePool(WithComponentWithSinglePropComponent),
    ETypePool(ComponentWithSinglePropComponent, Uri))

  val mongoContext = LongevityContext(subdomain, Mongo)
  val cassandraContext = LongevityContext(subdomain, Cassandra)

}