package emblem.testData

import com.github.nscala_time.time.Imports.richDateTime
import emblem.Emblem
import emblem.EmblemPool
import emblem.Emblematic
import emblem.Extractor
import emblem.ExtractorPool
import emblem.HasEmblem
import emblem.Union
import emblem.UnionPool
import emblem.traversors.sync.TestDataGenerator
import emblem.typeKey
import org.joda.time.DateTime

/** an attempt at an exhaustive set of data to cover emblematic traversal logic */
object exhaustive {

  case class Email(email: String)
  case class Markdown(markdown: String)
  case class Uri(uri: String)

  val emailExtractor = Extractor[Email, String]
  val markdownExtractor = Extractor[Markdown, String]
  val uriExtractor = Extractor[Uri, String]

  lazy val extractorPool = ExtractorPool(emailExtractor, markdownExtractor, uriExtractor)

  case class WithBasics(
    boolean: Boolean,
    char: Char,
    dateTime: DateTime,
    double: Double,
    float: Float,
    int: Int,
    long: Long,
    string: String)
  extends HasEmblem

  case class WithExtractors(
    email: Email,
    markdown: Markdown,
    uri: Uri)
  extends HasEmblem

  case class WithCollections(
    option: Option[String],
    set: Set[String],
    list: List[String])
  extends HasEmblem

  trait WithSpecialization {
    val common: String
  }

  case class Specialization1(
    common: String,
    special1: String)
  extends WithSpecialization with HasEmblem

  case class Specialization2(
    common: String,
    special2: String)
  extends WithSpecialization with HasEmblem

  lazy val emblemPool = EmblemPool(
    Emblem[WithBasics],
    Emblem[WithExtractors],
    Emblem[WithCollections],
    Emblem[Specialization1],
    Emblem[Specialization2])

  lazy val unionPool = UnionPool(
    Union[WithSpecialization](typeKey[Specialization1], typeKey[Specialization2]))

  lazy val emblematic = Emblematic(extractorPool, emblemPool, unionPool)

  lazy val generator = new TestDataGenerator(emblematic)

  object basics {
    def boolean = generator.generate[Boolean]
    def char = generator.generate[Char]
    def dateTime = generator.generate[DateTime]
    def double = generator.generate[Double]
    def float = generator.generate[Float]
    def int = generator.generate[Int]
    def long = generator.generate[Long]
    def string = generator.generate[String]
  }

  object extractors {
    def email = generator.generate[Email]
    def markdown = generator.generate[Markdown]
    def uri = generator.generate[Uri]
  }

  object emblems {
    def withBasics = generator.generate[WithBasics]
    def withExtractors = generator.generate[WithExtractors]
    def withCollections = generator.generate[WithCollections]
    def specialization1 = generator.generate[Specialization1]
    def specialization2 = generator.generate[Specialization2]
  }

  object unions {
    def withSpecialization = generator.generate[WithSpecialization]
  }

  object options {
    def boolean = generator.generate[Option[Boolean]]
    def char = generator.generate[Option[Char]]
    def dateTime = generator.generate[Option[DateTime]]
    def double = generator.generate[Option[Double]]
    def float = generator.generate[Option[Float]]
    def int = generator.generate[Option[Int]]
    def long = generator.generate[Option[Long]]
    def string = generator.generate[Option[String]]

    def email = generator.generate[Option[Email]]
    def markdown = generator.generate[Option[Markdown]]
    def uri = generator.generate[Option[Uri]]

    def withBasics = generator.generate[Option[WithBasics]]
    def withExtractors = generator.generate[Option[WithExtractors]]
    def withCollections = generator.generate[Option[WithCollections]]

    def option = generator.generate[Option[Option[String]]]
    def set = generator.generate[Option[Set[String]]]
    def list = generator.generate[Option[List[String]]]
  }

  object sets {
    def boolean = generator.generate[Set[Boolean]]
    def char = generator.generate[Set[Char]]
    def dateTime = generator.generate[Set[DateTime]]
    def double = generator.generate[Set[Double]]
    def float = generator.generate[Set[Float]]
    def int = generator.generate[Set[Int]]
    def long = generator.generate[Set[Long]]
    def string = generator.generate[Set[String]]

    def email = generator.generate[Set[Email]]
    def markdown = generator.generate[Set[Markdown]]
    def uri = generator.generate[Set[Uri]]

    def withBasics = generator.generate[Set[WithBasics]]
    def withExtractors = generator.generate[Set[WithExtractors]]
    def withCollections = generator.generate[Set[WithCollections]]

    def option = generator.generate[Set[Option[String]]]
    def set = generator.generate[Set[Set[String]]]
    def list = generator.generate[Set[List[String]]]
  }

  object lists {
    def boolean = generator.generate[List[Boolean]]
    def char = generator.generate[List[Char]]
    def dateTime = generator.generate[List[DateTime]]
    def double = generator.generate[List[Double]]
    def float = generator.generate[List[Float]]
    def int = generator.generate[List[Int]]
    def long = generator.generate[List[Long]]
    def string = generator.generate[List[String]]

    def email = generator.generate[List[Email]]
    def markdown = generator.generate[List[Markdown]]
    def uri = generator.generate[List[Uri]]

    def withBasics = generator.generate[List[WithBasics]]
    def withExtractors = generator.generate[List[WithExtractors]]
    def withCollections = generator.generate[List[WithCollections]]

    def option = generator.generate[List[Option[String]]]
    def set = generator.generate[List[Set[String]]]
    def list = generator.generate[List[List[String]]]
  }

}
