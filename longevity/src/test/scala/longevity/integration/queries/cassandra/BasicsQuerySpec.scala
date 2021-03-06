package longevity.integration.queries.cassandra

import com.github.nscala_time.time.Imports._
import longevity.TestLongevityConfigs
import longevity.context.LongevityContext
import longevity.exceptions.persistence.cassandra.FilterAllInQueryException
import longevity.exceptions.persistence.cassandra.NeqInQueryException
import longevity.exceptions.persistence.cassandra.OrInQueryException
import longevity.integration.model.basics._
import longevity.model.query.FilterAll
import longevity.model.query.Query
import longevity.test.ExerciseAkkaStreams
import longevity.test.ExerciseFS2
import longevity.test.ExerciseIterateeIo
import longevity.test.ExercisePlayEnumerator
import longevity.test.QuerySpec
import longevity.integration.queries.queryTestsExecutionContext
import scala.concurrent.Future

class BasicsQuerySpec extends QuerySpec[Future, DomainModel, Basics](
  new LongevityContext(TestLongevityConfigs.cassandraConfig))
    with ExerciseAkkaStreams[Future, DomainModel, Basics]
    with ExerciseFS2[Future, DomainModel, Basics]
    with ExerciseIterateeIo[Future, DomainModel, Basics]
    with ExercisePlayEnumerator[Future, DomainModel, Basics] {

  lazy val sample = randomP

  val booleanProp = Basics.props.boolean
  val charProp = Basics.props.char
  val doubleProp = Basics.props.double
  val floatProp = Basics.props.float
  val intProp = Basics.props.int
  val longProp = Basics.props.long
  val stringProp = Basics.props.string
  val dateTimeProp = Basics.props.dateTime

  import Basics.queryDsl._

  behavior of "CassandraPRepo.queryToVector"

  it should "produce expected results for Query.FilterAll" in {
    intercept[FilterAllInQueryException] {
      effect.run(repo.queryToVector(Query(FilterAll())))
    }
  }

  it should "produce expected results for simple equality queries" in {
    // only eqs here dur to cassandra query limitations
    exerciseQuery(booleanProp eqs sample.boolean, true)
    exerciseQuery(charProp eqs sample.char)
    exerciseQuery(dateTimeProp eqs sample.dateTime)
    exerciseQuery(doubleProp eqs sample.double)
    exerciseQuery(floatProp eqs sample.float)
    exerciseQuery(intProp eqs sample.int)
    exerciseQuery(longProp eqs sample.long)
    exerciseQuery(stringProp eqs sample.string)


    // make sure Query.FilterAll() can occur inside greater expression
    val query: Query[Basics] = stringProp eqs sample.string and FilterAll()
    intercept[FilterAllInQueryException] {
      effect.run(repo.queryToVector(query))
    }
  }

  it should "produce expected results for simple conditional queries" in {
    exerciseQuery(floatProp eqs sample.float and booleanProp lt !sample.boolean, true)
    exerciseQuery(floatProp eqs sample.float and charProp lte sample.char)
    exerciseQuery(floatProp eqs sample.float and dateTimeProp gt sample.dateTime - 1.day)
    exerciseQuery(floatProp eqs sample.float and doubleProp gte sample.double)
    exerciseQuery(longProp eqs sample.long and floatProp lt sample.float + 2.0f)
    exerciseQuery(floatProp eqs sample.float and intProp lte sample.int)
    exerciseQuery(floatProp eqs sample.float and longProp gt sample.long - 1)
    exerciseQuery(floatProp eqs sample.float and stringProp gte sample.string)
  }

  it should "produce expected results for nested conditional queries" in {
    exerciseQuery(
      booleanProp eqs sample.boolean and
      charProp lte sample.char and
      dateTimeProp lt sample.dateTime + 1.hour,
      true)
    exerciseQuery(
      dateTimeProp eqs sample.dateTime and (
        doubleProp gte sample.double and
        floatProp lt sample.float + 7),
      true)
    exerciseQuery(
      floatProp eqs sample.float and
      intProp lte sample.int and
      longProp gt sample.long - 2 and
      stringProp gte sample.string,
      true)
  }

  it should "throw exception for or queries" in {
    intercept[OrInQueryException] {
      effect.run(repo.queryToVector(
        booleanProp eqs sample.boolean or
          charProp lte sample.char
      ))
    }
  }

  it should "throw exception for neq queries" in {
    intercept[NeqInQueryException] {
      effect.run {
        repo.queryToVector(booleanProp neq sample.boolean)
      }
    }
  }

}
