package longevity.integration.model.partitionKeyWithPartialPartition

import longevity.model.annotations.keyVal

@keyVal[PartitionKeyWithPartialPartition]
case class Key(prop1: String, prop2: String)