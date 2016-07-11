package longevity.integration.subdomain

import longevity.context.LongevityContext
import longevity.context.Cassandra
import longevity.context.Mongo
import longevity.subdomain.Subdomain
import longevity.subdomain.embeddable.ETypePool
import longevity.subdomain.ptype.PTypePool

package object shorthands {

  val subdomain = Subdomain(
    "Shorthands",
    PTypePool(Shorthands),
    ETypePool(
      BooleanShorthand,
      CharShorthand,
      DateTimeShorthand,
      DoubleShorthand,
      FloatShorthand,
      IntShorthand,
      LongShorthand,
      StringShorthand))

  val mongoContext = LongevityContext(subdomain, Mongo)
  val cassandraContext = LongevityContext(subdomain, Cassandra)

}
