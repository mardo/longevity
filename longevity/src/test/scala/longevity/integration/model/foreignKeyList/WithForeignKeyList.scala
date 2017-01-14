package longevity.integration.model.foreignKeyList

import longevity.model.annotations.persistent

@persistent(keySet = Set(key(props.id)))
case class WithForeignKeyList(
  id: WithForeignKeyListId,
  associated: List[AssociatedId])