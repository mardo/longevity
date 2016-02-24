package emblem.traversors.sync

import com.github.nscala_time.time.Imports._
import emblem.imports._
import emblem.testData.geometry
import org.json4s.JsonAST._
import org.scalatest._

/** specs for [[JsonToEmblemTranslator]] */
class JsonToEmblemTranslatorSpec extends FlatSpec with GivenWhenThen with Matchers {

  private val translator = new JsonToEmblemTranslator {
    override protected val emblemPool: EmblemPool = geometry.emblemPool
  }

  behavior of "JsonToEmblemTranslator.generate[A] for basic types"

  it should "produce the appropriate json4s values" in {
    translator.traverse[Boolean](JBool(true)) should equal (true)
    translator.traverse[Boolean](JBool(false)) should equal (false)
    translator.traverse[Char](JString("q")) should equal ('q')
    val dt = DateTime.now
    translator.traverse[DateTime](JString(translator.formatter.print(dt))) should equal (dt)
    translator.traverse[Double](JDouble(0.7d)) should equal (0.7d)
    translator.traverse[Float](JDouble(0.7f)) should equal (0.7f)
    translator.traverse[Int](JInt(9)) should equal (9)
    translator.traverse[Long](JLong(9L)) should equal (9L)
    translator.traverse[String](JString("string")) should equal ("string")
  }

  behavior of "JsonToEmblemTranslator.generate[Point]"

  it should "produce a Point from json4s" in {
    { translator.traverse[geometry.Point](
      JObject(List("x" -> JDouble(0.4d), "y" -> JDouble(-0.3d))))
    } should equal {
      geometry.Point(0.4d, -0.3d)
    }
  }

  behavior of "JsonToEmblemTranslator.generate[Polygon]"

  it should "produce a Polygon from json4s" in {
    { translator.traverse[geometry.Polygon](
      JObject(List("corners" -> JArray(List(
        JObject(List("x" -> JDouble(0.44d), "y" -> JDouble(-0.34d))),
        JObject(List("x" -> JDouble(0.45d), "y" -> JDouble(-0.35d))),
        JObject(List("x" -> JDouble(0.46d), "y" -> JDouble(-0.36d))))))))
    } should equal {
      geometry.Polygon(Set(
        geometry.Point(0.44d, -0.34d),
        geometry.Point(0.45d, -0.35d),
        geometry.Point(0.46d, -0.36d)))
    }
  }

}
