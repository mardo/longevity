package longevity.integration.model.component

import longevity.model.annotations.persistent

@persistent[DomainModel]
case class WithComponent(
  id: WithComponentId,
  foo: String,
  component: Component)

object WithComponent {
  implicit val idKey = key(props.id)
  override val indexSet = Set(index(props.component), index(props.foo))
}
