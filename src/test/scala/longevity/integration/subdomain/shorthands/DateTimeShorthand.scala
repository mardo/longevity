package longevity.integration.subdomain.shorthands

import longevity.subdomain.embeddable.ValueObject
import longevity.subdomain.embeddable.ValueType
import org.joda.time.DateTime

case class DateTimeShorthand(dateTime: DateTime) extends ValueObject

object DateTimeShorthand extends ValueType[DateTimeShorthand]
