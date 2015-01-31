package emblem.testData

import emblem._

/** for testing emblem success cases */
object geometry {

  case class Point(x: Double, y: Double) extends HasEmblem
  lazy val pointEmblem = emblemFor[Point]
  lazy val xProp = pointEmblem.prop[Double]("x")
  lazy val yProp = pointEmblem.prop[Double]("y")

  case class Polygon(corners: Set[Point]) extends HasEmblem
  lazy val polygonEmblem = emblemFor[Polygon]
  lazy val cornersProp = polygonEmblem.prop[Set[Point]]("corners")

  case class PointWithDefaults(x: Double = 17.0, y: Double = 13.0) extends HasEmblem
  lazy val pointWithDefaultsEmblem = emblemFor[PointWithDefaults]
  lazy val xPropWithDefaults = pointWithDefaultsEmblem.prop[Double]("x")
  lazy val yPropWithDefaults = pointWithDefaultsEmblem.prop[Double]("y")

}
