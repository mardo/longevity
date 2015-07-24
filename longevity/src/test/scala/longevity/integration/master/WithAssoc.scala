package longevity.integration.master

import longevity.subdomain._

case class WithAssoc(
  uri: String,
  associated: Assoc[Associated])
extends RootEntity

object WithAssoc extends RootEntityType[WithAssoc]
