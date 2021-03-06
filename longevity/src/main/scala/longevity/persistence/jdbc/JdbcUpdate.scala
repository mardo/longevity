package longevity.persistence.jdbc

import longevity.exceptions.persistence.WriteConflictException
import longevity.persistence.PState

/** implementation of JdbcPRepo.update */
private[jdbc] trait JdbcUpdate[F[_], M, P] {
  repo: JdbcPRepo[F, M, P] =>

  override def update(state: PState[P]): F[PState[P]] = {
    val fs = effect.pure(state)
    val fss = effect.map(fs) { state =>
      logger.debug(s"executing JdbcPRepo.update: $state")
      validateStablePrimaryKey(state)
      val newState = state.update(persistenceConfig.optimisticLocking, persistenceConfig.writeTimestamps)
      (state, newState)
    }
    val fssr = effect.mapBlocking(fss) { case (state, newState) =>
      val rowCount = try {
        bindUpdateStatement(newState, state.rowVersionOrNull).executeUpdate()
      } catch {
        convertDuplicateKeyException(newState)
      }
      (state, newState, rowCount)
    }
    effect.map(fssr) { case (state, newState, rowCount) =>
      if (persistenceConfig.optimisticLocking && rowCount != 1) {
        throw new WriteConflictException(state)
      }
      logger.debug(s"done executing JdbcPRepo.update: $newState")
      newState
    }
  }

  private def bindUpdateStatement(state: PState[P], rowVersion: AnyRef) = {
    val columnBindings = if (persistenceConfig.optimisticLocking) {
      updateColumnValues(state, isCreate = false) ++: whereBindings(state) :+ rowVersion
    } else {
      updateColumnValues(state, isCreate = false) ++: whereBindings(state)
    }
    logger.debug(s"invoking SQL: $updateSql with bindings: $columnBindings")
    val preparedStatement = connection().prepareStatement(updateSql)
    columnBindings.zipWithIndex.foreach { case (binding, index) =>
      preparedStatement.setObject(index + 1, binding)
    }
    preparedStatement
  }

  private def updateSql = if (persistenceConfig.optimisticLocking) {
    withLockingUpdateSql
  } else {
    withoutLockingUpdateSql
  }

  private def columnAssignments = updateColumnNames(isCreate = false).map(c => s"$c = :$c").mkString(",\n  ")

  private def withoutLockingUpdateSql = s"""|
  |UPDATE $tableName
  |SET
  |  $columnAssignments
  |WHERE
  |  $whereAssignments
  |""".stripMargin

  private def withLockingUpdateSql = s"""|$withoutLockingUpdateSql
  |AND
  |  row_version = :old_row_version
  |""".stripMargin

}
