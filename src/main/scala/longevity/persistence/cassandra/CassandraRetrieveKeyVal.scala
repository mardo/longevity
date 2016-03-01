package longevity.persistence.cassandra

import com.datastax.driver.core.BoundStatement
import com.datastax.driver.core.PreparedStatement
import com.datastax.driver.core.Row
import com.datastax.driver.core.Session
import java.util.UUID
import longevity.persistence._
import longevity.subdomain._
import longevity.subdomain.root._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/** implementation of CassandraRepo.retrieve(KeyVal) */
private[cassandra] trait CassandraRetrieveKeyVal[R <: Root] {
  repo: CassandraRepo[R] =>

  override def retrieve(keyVal: KeyVal[R]): Future[Option[PState[R]]] =
    retrieveFromBoundStatement(bindKeyValSelectStatement(keyVal))

  private lazy val keyValSelectStatement: Map[Key[R], PreparedStatement] = Map().withDefault { key =>
    val relations = key.props.map(columnName).map(name => s"$name = :$name").mkString("\nAND\n  ")
    val cql = s"""|
    |SELECT * FROM $tableName
    |WHERE
    |  $relations
    |ALLOW FILTERING
    |""".stripMargin
    session.prepare(cql)
  }

  private def bindKeyValSelectStatement(keyVal: KeyVal[R]): BoundStatement = {
    val preparedStatement = keyValSelectStatement(keyVal.key)
    val propVals = keyVal.key.props.map { prop =>
      def bind[A](prop: Prop[R, A]) = cassandraValue(keyVal(prop))(prop.typeKey)
      bind(prop)
    }
    preparedStatement.bind(propVals: _*)
  }

}
