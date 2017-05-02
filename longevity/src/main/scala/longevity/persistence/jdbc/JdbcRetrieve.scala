package longevity.persistence.jdbc

import java.sql.PreparedStatement
import emblem.TypeKey
import longevity.model.KeyVal
import longevity.model.realized.RealizedKey
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.blocking

/** implementation of JdbcRepo.retrieve(KeyVal) */
private[jdbc] trait JdbcRetrieve[P] {
  repo: JdbcRepo[P] =>

  def retrieve[V <: KeyVal[P] : TypeKey](keyVal: V)(implicit context: ExecutionContext) = {
    logger.debug(s"calling JdbcRepo.retrieve: $keyVal")
    val stateOption = retrieveFromPreparedStatement(bindKeyValSelectStatement(keyVal))
    logger.debug(s"done calling JdbcRepo.retrieve: $stateOption")
    stateOption
  }

  protected def retrieveFromPreparedStatement(statement: PreparedStatement)(implicit context: ExecutionContext) =
    Future {
      val resultSet = blocking { statement.executeQuery() }
      if (resultSet.next()) {
        Some(retrieveFromResultSet(resultSet))
      } else {
        None
      }
    }

  private def bindKeyValSelectStatement[V <: KeyVal[P] : TypeKey](keyVal: V) = {
    val realizedKey: RealizedKey[P, V] = realizedPType.realizedKey[V]
    val propVals = realizedKey.realizedProp.realizedPropComponents.map { component =>
      jdbcValue(component.innerPropPath.get(keyVal))
    }
    val sql = keyValSelectStatement(realizedKey)
    val preparedStatement = connection.prepareStatement(sql)
    logger.debug(s"invoking SQL: $sql with bindings: $propVals")
    propVals.zipWithIndex.foreach { case (propVal, index) =>
      preparedStatement.setObject(index + 1, propVal)
    }
    preparedStatement
  }

  private lazy val keyValSelectStatement: Map[RealizedKey[P, _], String] = Map().withDefault { key =>
    val conjunction = keyValSelectStatementConjunction(key)
    s"""|
    |SELECT * FROM $tableName
    |WHERE
    |  $conjunction
    |""".stripMargin
  }

  protected def keyValSelectStatementConjunction(key: RealizedKey[P, _]): String = {
    key.realizedProp.realizedPropComponents.map(columnName).map(name => s"$name = :$name").mkString("\nAND\n  ")
  }

}