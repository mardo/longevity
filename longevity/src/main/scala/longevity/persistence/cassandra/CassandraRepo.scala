package longevity.persistence.cassandra

import com.datastax.driver.core.Cluster
import com.datastax.driver.core.Session
import com.datastax.driver.core.exceptions.InvalidQueryException
import longevity.config.CassandraConfig
import longevity.config.PersistenceConfig
import longevity.effect.Effect
import longevity.exceptions.persistence.ConnectionClosedException
import longevity.exceptions.persistence.ConnectionOpenException
import longevity.exceptions.persistence.cassandra.KeyspaceDoesNotExistException
import longevity.model.ModelType
import longevity.model.PType
import longevity.persistence.Repo

private[persistence] class CassandraRepo[F[_], M] private[persistence](
  effect: Effect[F],
  modelType: ModelType[M],
  persistenceConfig: PersistenceConfig,
  private val cassandraConfig: CassandraConfig)
extends Repo[F, M](effect, modelType, persistenceConfig) {

  type R[P] = CassandraPRepo[F, M, P]

  private var sessionOpt: Option[Session] = None

  private lazy val session = () => sessionOpt match {
    case Some(s) => s
    case None => throw new ConnectionClosedException
  }

  protected def buildPRepo[P](pType: PType[M, P], polyRepoOpt: Option[R[_ >: P]] = None): R[P] =
    CassandraPRepo[F, M, P](effect, modelType, pType, persistenceConfig, polyRepoOpt, session)

  protected def openBaseConnectionBlocking(): Unit = synchronized {
    if (sessionOpt.nonEmpty) {
      throw new ConnectionOpenException
    }
    val builder = Cluster.builder.addContactPoint(cassandraConfig.address)
    val cluster = {
      cassandraConfig.credentials.foreach { creds =>
        builder.withCredentials(creds.username, creds.password)
      }
      builder.build
    }

    val session = cluster.connect()

    if (cassandraConfig.autoCreateKeyspace) {
      session.execute(
        s"""|
            |CREATE KEYSPACE IF NOT EXISTS ${cassandraConfig.keyspace}
            |WITH replication = {
            |  'class': 'SimpleStrategy',
            |  'replication_factor': ${cassandraConfig.replicationFactor}
            |};
            |""".stripMargin)
    }

    try {
      session.execute(s"use ${cassandraConfig.keyspace}")
    } catch {
      case e: InvalidQueryException if
        e.getMessage.startsWith("Keyspace '") &&
        e.getMessage.endsWith("' does not exist") =>
        throw new KeyspaceDoesNotExistException(cassandraConfig, e)
    }

    sessionOpt = Some(session)
  }

  protected def createBaseSchemaBlocking(): Unit = ()

  protected def closeConnectionBlocking(): Unit = synchronized {
    if (sessionOpt.isEmpty) throw new ConnectionClosedException
    sessionOpt.get.close()
    sessionOpt.get.getCluster.close()
    sessionOpt = None
  }

}
