package longevity.unit.model.annotations

import typekey.typeKey
import longevity.model.PType
import longevity.model.annotations.persistent
import org.scalatest.FlatSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers

/** unit tests for the proper behavior of [[mprops `@persistent` macro annotation]] */
class PersistentSpec extends FlatSpec with GivenWhenThen with Matchers {

  import testExamples._

  behavior of "@persistent"

  it should "cause a compiler error when annotating something other than a class, trait, or object" in {
    "@persistent[DomainModel] val x = 7"           shouldNot compile
    "@persistent[DomainModel] type X = Int"        shouldNot compile
    "@persistent[DomainModel] def foo = 7"         shouldNot compile
    "def foo(@persistent[DomainModel] x: Int) = 7" shouldNot compile
    "@persistent[DomainModel] trait Foo"           shouldNot compile
    "@persistent[DomainModel] object Foo"          shouldNot compile
  }

  it should "create a companion object that extends PType when there is no companion object" in {
    PNoCompanion.isInstanceOf[PType[DomainModel, PNoCompanion]] should be (true)
    PNoCompanion.asInstanceOf[PType[DomainModel, PNoCompanion]].pTypeKey should equal {
      typeKey[PNoCompanion]
    }
  }

  it should "augment an existing companion object to extend PType" in {
    PWithCompanion.isInstanceOf[PType[DomainModel, PWithCompanion]] should be (true)
    PWithCompanion.asInstanceOf[PType[DomainModel, PWithCompanion]].pTypeKey should equal {
      typeKey[PWithCompanion]
    }
    PWithCompanion.y should equal (7)

    PCaseClass.isInstanceOf[PType[DomainModel, PCaseClass]] should be (true)

    PCaseClass.asInstanceOf[PType[DomainModel, PCaseClass]].pTypeKey should equal {
      typeKey[PCaseClass]
    }
    PCaseClass.apply() should equal (PCaseClass())

    PCaseClassWithDefaults.isInstanceOf[PType[DomainModel, PCaseClassWithDefaults]] should be (true)

    PCaseClassWithDefaults.asInstanceOf[PType[DomainModel, PCaseClassWithDefaults]].pTypeKey should equal {
      typeKey[PCaseClassWithDefaults]
    }
    PCaseClassWithDefaults.apply(3) should equal (PCaseClassWithDefaults(3))
  }

}
