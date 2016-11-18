package longevity.integration.subdomain.basicOptions

import com.github.nscala_time.time.Imports._
import longevity.subdomain.PType
import longevity.subdomain.mprops

case class BasicOptions(
  id: BasicOptionsId,
  boolean: Option[Boolean],
  char: Option[Char],
  double: Option[Double],
  float: Option[Float],
  int: Option[Int],
  long: Option[Long],
  string: Option[String],
  dateTime: Option[DateTime])

@mprops object BasicOptions extends PType[BasicOptions] {
  object keys {
    val id = key(props.id)
  }
}
