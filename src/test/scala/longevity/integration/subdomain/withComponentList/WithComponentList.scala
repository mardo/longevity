package longevity.integration.subdomain.withComponentList

import longevity.subdomain.persistent.Root
import longevity.subdomain.ptype.RootType

case class WithComponentList(
  uri: String,
  components: List[Component])
extends Root

object WithComponentList extends RootType[WithComponentList] {
  object props {
    val uri = prop[String]("uri")
  }
  object keys {
    val uri = key(props.uri)
  }
  object indexes {
  }
}