package longevity.integration.model.keyInComponent

import longevity.model.annotations.persistent

@persistent(keySet = Set(key(props.component.key)))
case class KeyInComponent(
  filler: String,
  component: Component)