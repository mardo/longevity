package longevity.integration.queries.inmem

import longevity.TestLongevityConfigs
import longevity.context.LongevityContext
import longevity.test.QuerySpec
import longevity.integration.model.component._
import longevity.integration.queries.queryTestsExecutionContext

class ComponentQuerySpec extends QuerySpec[WithComponent](
  new LongevityContext(domainModel, TestLongevityConfigs.inMemConfig)) {

  lazy val sample = randomP

  val componentProp = WithComponent.props.component

  import WithComponent.queryDsl._

  behavior of "InMemRepo.retrieveByQuery"

  it should "produce expected results for simple equality queries" in {
    exerciseQuery(componentProp eqs sample.component)
    exerciseQuery(componentProp neq sample.component)
  }

  it should "produce expected results for simple ordering queries" in {
    exerciseQuery(componentProp gt sample.component)
    exerciseQuery(componentProp gte sample.component)
    exerciseQuery(componentProp lt sample.component)
    exerciseQuery(componentProp lte sample.component)
  }

}
