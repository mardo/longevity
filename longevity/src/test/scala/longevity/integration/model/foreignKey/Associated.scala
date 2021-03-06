package longevity.integration.model.foreignKey

import longevity.model.annotations.persistent

@persistent[DomainModel]
case class Associated(id: AssociatedId)

object Associated {
  implicit val idKey = key(props.id)
}
