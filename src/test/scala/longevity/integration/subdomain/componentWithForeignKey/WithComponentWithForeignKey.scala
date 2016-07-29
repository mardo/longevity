package longevity.integration.subdomain.componentWithForeignKey

import longevity.subdomain.persistent.Root
import longevity.subdomain.ptype.RootType

case class WithComponentWithForeignKey(
  id: WithComponentWithForeignKeyId,
  component: ComponentWithForeignKey)
extends Root

object WithComponentWithForeignKey extends RootType[WithComponentWithForeignKey] {
  object props {
    val id = prop[WithComponentWithForeignKeyId]("id")
  }
  object keys {
    val id = key(props.id)
  }
}
