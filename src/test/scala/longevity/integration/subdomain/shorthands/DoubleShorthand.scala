package longevity.integration.subdomain.shorthands

import longevity.subdomain.embeddable.ValueObject
import longevity.subdomain.embeddable.ValueType

case class DoubleShorthand(double: Double) extends ValueObject

object DoubleShorthand extends ValueType[DoubleShorthand]
