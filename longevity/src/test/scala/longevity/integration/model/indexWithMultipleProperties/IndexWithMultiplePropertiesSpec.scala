package longevity.integration.model.indexWithMultipleProperties

import org.scalatest.Suites
import longevity.integration.model.modelTestsExecutionContext

class IndexWithMultiplePropertiesSpec extends Suites(contexts.map(_.repoCrudSpec): _*)
