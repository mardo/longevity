package emblem.testData

import emblem._

/** a handful of extractors used for testing */
object extractors {

  lazy val extractorPool =
    ExtractorPool(emailExtractor, markdownExtractor, radiansExtractor, uriExtractor, zipcodeExtractor)

  case class Email(email: String)
  lazy val emailExtractor = extractorFor[Email, String]

  case class Markdown(markdown: String)
  lazy val markdownExtractor = extractorFor[Markdown, String]

  case class Radians(radians: Double)
  lazy val radiansExtractor = extractorFor[Radians, Double]

  case class Uri(uri: String)
  lazy val uriExtractor = extractorFor[Uri, String]

  case class Zipcode(zipcode: Int)
  lazy val zipcodeExtractor = extractorFor[Zipcode, Int]

  // failure cases

  case class NoExtractor(noExtractor: String)

  trait Foo
  case class Bar(foo: Foo)
  lazy val barExtractor = extractorFor[Bar, Foo]

}
