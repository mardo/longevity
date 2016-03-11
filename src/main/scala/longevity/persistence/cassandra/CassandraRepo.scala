package longevity.persistence.cassandra

import com.datastax.driver.core.BoundStatement
import com.datastax.driver.core.PreparedStatement
import com.datastax.driver.core.Row
import com.datastax.driver.core.Session
import emblem.imports._
import emblem.jsonUtil.dateTimeFormatter
import emblem.stringUtil._
import java.util.UUID
import longevity.persistence._
import longevity.subdomain._
import longevity.subdomain.root._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/** a Cassandra repository for persistent entities of type `P`.
 *
 * @param pType the type of the persistent entities this repository handles
 * @param subdomain the subdomain containing the root that this repo persists
 * @param session the connection to the cassandra database
 */
private[longevity] class CassandraRepo[P <: Persistent : TypeKey] protected[persistence] (
  pType: PType[P],
  subdomain: Subdomain,
  protected val session: Session)
extends BaseRepo[P](pType, subdomain)
with CassandraSchema[P]
with CassandraCreate[P]
with CassandraRetrieveAssoc[P]
with CassandraRetrieveKeyVal[P]
with CassandraRetrieveQuery[P]
with CassandraUpdate[P]
with CassandraDelete[P] {

  protected val tableName = camelToUnderscore(typeName(pTypeKey.tpe))
  protected val realizedProps = pType.keySet.flatMap(_.props) ++ pType.indexSet.flatMap(_.props)
  protected val emblemPool = subdomain.entityEmblemPool
  protected val shorthandPool = subdomain.shorthandPool

  private val extractorPool = shorthandPoolToExtractorPool(shorthandPool)
  private val rootToJsonTranslator = new PersistentToJsonTranslator(emblemPool, extractorPool)
  private val jsonToRootTranslator = new JsonToPersistentTranslator(emblemPool, extractorPool)

  protected def columnName(prop: Prop[P, _]) = "prop_" + scoredPath(prop)

  protected def scoredPath(prop: Prop[P, _]) = prop.path.replace('.', '_')

  protected def jsonStringForRoot(p: P): String = {
    import org.json4s.native.JsonMethods._
    compact(render(rootToJsonTranslator.traverse(p)))
  }

  protected def propValBinding[A](prop: Prop[P, A], p: P): AnyRef = {
    def bind[B : TypeKey](prop: Prop[P, B]) = cassandraValue(prop.propVal(p))
    bind(prop)(prop.typeKey)
  }

  protected def cassandraValue[A : TypeKey](value: A): AnyRef = {
    val abbreviated = value match {
      case actual if shorthandPool.contains[A] => shorthandPool[A].abbreviate(actual)
      case a => a
    }
    abbreviated match {
      case id: CassandraId[_] => id.uuid
      case char: Char => char.toString
      case d: DateTime => dateTimeFormatter.print(d)
      case _ => abbreviated.asInstanceOf[AnyRef]
    }
  }

  protected def retrieveFromBoundStatement(
    statement: BoundStatement)(
    implicit context: ExecutionContext)
  : Future[Option[PState[P]]] =
    Future {
      val resultSet = session.execute(statement)
      val rowOption = Option(resultSet.one)
      rowOption.map(retrieveFromRow)
    }

  protected def retrieveFromRow(row: Row): PState[P] = {
    val id = CassandraId[P](row.getUUID("id"))
    import org.json4s.native.JsonMethods._    
    val json = parse(row.getString("root"))
    val p = jsonToRootTranslator.traverse[P](json)
    new PState[P](id, p)
  }

  createSchema()

}

private[cassandra] object CassandraRepo {

  private[cassandra] val basicToCassandraType = Map[TypeKey[_], String](
    typeKey[Boolean] -> "boolean",
    typeKey[Char] -> "text",
    typeKey[DateTime] -> "text",
    typeKey[Double] -> "double",
    typeKey[Float] -> "float",
    typeKey[Int] -> "int",
    typeKey[Long] -> "bigint",
    typeKey[String] -> "text")

}
