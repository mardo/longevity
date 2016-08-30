package longevity.integration.subdomain

import longevity.context.LongevityContext
import longevity.context.Cassandra
import longevity.context.Mongo
import longevity.subdomain.Subdomain
import longevity.subdomain.embeddable.ETypePool
import longevity.subdomain.embeddable.ValueType
import longevity.subdomain.ptype.PTypePool

package object shorthandOptions {

  val subdomain = Subdomain(
    "Shorthand Options",
    PTypePool(ShorthandOptions),
    ETypePool(
      ValueType[BooleanShorthand],
      ValueType[CharShorthand],
      ValueType[DateTimeShorthand],
      ValueType[DoubleShorthand],
      ValueType[FloatShorthand],
      ValueType[IntShorthand],
      ValueType[LongShorthand],
      ValueType[StringShorthand]))

  val mongoContext = LongevityContext(subdomain, Mongo)
  val cassandraContext = LongevityContext(subdomain, Cassandra)

}
