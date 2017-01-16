package longevity.unit.model

import longevity.exceptions.model.ptype.MultiplePrimaryKeysForPType
import longevity.exceptions.model.ptype.NoPropsForPTypeException
import longevity.exceptions.model.ptype.PrimaryKeyForDerivedPTypeException
import longevity.model.PType
import org.scalatest.FlatSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers

/** holds domain objects for special case PrimaryKeyForDerivedPTypeException */
object PTypeSpec {

  import longevity.model.KeyVal
  import longevity.model.DerivedPType
  import longevity.model.PolyPType

  case class Username(username: String) extends KeyVal[User]

  trait User {
    val username: Username
  }

  object User extends PolyPType[User] {
    object props {
      val username = prop[Username]("username")
    }
    val keySet = Set(primaryKey(props.username))
  }

  case class Email(email: String) extends KeyVal[EmailedUser]

  case class EmailedUser(username: Username, email: Email) extends User

  object EmailedUser extends DerivedPType[EmailedUser, User] {
    object props {
      val email = prop[Email]("email")
    }
    val keySet = Set(primaryKey(props.email))
  }

}

/** unit tests for the proper construction and behavior of a [[PType persistent type]] */
class PTypeSpec extends FlatSpec with GivenWhenThen with Matchers {

  import longevity.unit.blogCore._

  behavior of "PType construction"

  it should "throw exception when the `propSet` is not overridden, and there is no `object props`" in {
    object User extends PType[User] {
      val keySet = emptyKeySet
    }
    intercept[NoPropsForPTypeException[_]] {
      User.propSet
    }
  }

  it should "produce an empty `propSet` when `object props` is empty" in {
    object User extends PType[User] {
      object props {
      }
      val keySet = emptyKeySet
    }
    User.propSet should equal (Set())
  }

  it should "produce a non-empty `propSet` when `object props` holds props of the right type" in {
    object User extends PType[User] {
      object props {
        val username = prop[Username]("username")
        val email = prop[Email]("email")
      }
      val keySet = emptyKeySet
    }
    User.propSet should equal (Set(User.props.username, User.props.email))
  }

  it should "throw exception if more than one primary key is defined" in {
    object User extends PType[User] {
      object props {
        val username = prop[Username]("username")
        val email = prop[Email]("email")
      }
      val keySet = Set(primaryKey(props.username), primaryKey(props.email))
    }
    intercept[MultiplePrimaryKeysForPType[_]] {
      User.primaryKey
    }
  }

  it should "throw exception if a derived ptype defines primary key" in {
    intercept[PrimaryKeyForDerivedPTypeException[_]] {
      PTypeSpec.EmailedUser.primaryKey
    }
  }

  it should "produce an empty `indexSet` when the `indexSet` is not overridden, " +
  "and there is no `object indexes`" in {
    object User extends PType[User] {
      object props {
      }
      val keySet = emptyKeySet
    }
    User.indexSet should equal (Set())
  }

  behavior of "PType.toString"

  it should "produce a string indicating its a PType and what the Persistent type is" in {
    object User extends PType[User] {
      object props {
      }
      val keySet = emptyKeySet
    }
    User.toString should equal ("PType[User]")
  }

}