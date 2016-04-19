package emblem.traversors.sync

import emblem.TypeKey
import emblem.testData.exhaustive
import org.scalatest.FlatSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers

/** exercises translations of [[JsonToEmblemTranslator]] and
 * [[EmblemToJsonTranslator]] by running data through both, and seeing if we
 * get the original result. also exercises the [[TestDataGenerator]].
 *
 * this test attempts to be exhaustive of emblematic features using
 * [[emblem.testData.exhaustive]].
 */
class JsonTranslationSpec extends FlatSpec with GivenWhenThen with Matchers {

  private val emblemTranslator = new EmblemToJsonTranslator {
    override protected val emblematic = exhaustive.emblematic
  }

  private val jsonTranslator = new JsonToEmblemTranslator {
    override protected val emblematic = exhaustive.emblematic
  }

  behavior of "a pipeline of the JSON producer and consumer translators"

  it should "produce the original output for basic types" in {
    pipelineReproducesInput(exhaustive.basics.boolean)
    pipelineReproducesInput(exhaustive.basics.char)
    pipelineReproducesInput(exhaustive.basics.dateTime)
    pipelineReproducesInput(exhaustive.basics.double)
    pipelineReproducesInput(exhaustive.basics.float)
    pipelineReproducesInput(exhaustive.basics.int)
    pipelineReproducesInput(exhaustive.basics.long)
    pipelineReproducesInput(exhaustive.basics.string)    
  }

  it should "produce the original output for extractors" in {
    pipelineReproducesInput(exhaustive.extractors.email)
    pipelineReproducesInput(exhaustive.extractors.markdown)
    pipelineReproducesInput(exhaustive.extractors.uri)
  }

  it should "produce the original output for emblems" in {
    pipelineReproducesInput(exhaustive.emblems.withBasics)
    pipelineReproducesInput(exhaustive.emblems.withExtractors)
    pipelineReproducesInput(exhaustive.emblems.withCollections)
    pipelineReproducesInput(exhaustive.emblems.specialization1)
    pipelineReproducesInput(exhaustive.emblems.specialization2)
  }

  it should "produce the original output for unions" in {

    println(emblemTranslator.traverse(exhaustive.unions.withSpecialization))

    pipelineReproducesInput(exhaustive.unions.withSpecialization)

    // TODO: more
  }

  it should "produce the original output for options" in {
    pipelineReproducesInput(exhaustive.options.boolean)
    pipelineReproducesInput(exhaustive.options.char)
    pipelineReproducesInput(exhaustive.options.dateTime)
    pipelineReproducesInput(exhaustive.options.double)
    pipelineReproducesInput(exhaustive.options.float)
    pipelineReproducesInput(exhaustive.options.int)
    pipelineReproducesInput(exhaustive.options.long)
    pipelineReproducesInput(exhaustive.options.string)

    pipelineReproducesInput(exhaustive.options.email)
    pipelineReproducesInput(exhaustive.options.markdown)
    pipelineReproducesInput(exhaustive.options.uri)

    pipelineReproducesInput(exhaustive.options.withBasics)
    pipelineReproducesInput(exhaustive.options.withExtractors)
    pipelineReproducesInput(exhaustive.options.withCollections)

    pipelineReproducesInput(exhaustive.options.option.flatten) // JSON rep does not respect Some(None)
    pipelineReproducesInput(exhaustive.options.set)
    pipelineReproducesInput(exhaustive.options.list)
  }

  it should "produce the original output for sets" in {
    pipelineReproducesInput(exhaustive.sets.boolean)
    pipelineReproducesInput(exhaustive.sets.char)
    pipelineReproducesInput(exhaustive.sets.dateTime)
    pipelineReproducesInput(exhaustive.sets.double)
    pipelineReproducesInput(exhaustive.sets.float)
    pipelineReproducesInput(exhaustive.sets.int)
    pipelineReproducesInput(exhaustive.sets.long)
    pipelineReproducesInput(exhaustive.sets.string)

    pipelineReproducesInput(exhaustive.sets.email)
    pipelineReproducesInput(exhaustive.sets.markdown)
    pipelineReproducesInput(exhaustive.sets.uri)

    pipelineReproducesInput(exhaustive.sets.withBasics)
    pipelineReproducesInput(exhaustive.sets.withExtractors)
    pipelineReproducesInput(exhaustive.sets.withCollections)

    pipelineReproducesInput(exhaustive.sets.option)
    pipelineReproducesInput(exhaustive.sets.set)
    pipelineReproducesInput(exhaustive.sets.list)
  }

  it should "produce the original output for lists" in {
    pipelineReproducesInput(exhaustive.lists.boolean)
    pipelineReproducesInput(exhaustive.lists.char)
    pipelineReproducesInput(exhaustive.lists.dateTime)
    pipelineReproducesInput(exhaustive.lists.double)
    pipelineReproducesInput(exhaustive.lists.float)
    pipelineReproducesInput(exhaustive.lists.int)
    pipelineReproducesInput(exhaustive.lists.long)
    pipelineReproducesInput(exhaustive.lists.string)

    pipelineReproducesInput(exhaustive.lists.email)
    pipelineReproducesInput(exhaustive.lists.markdown)
    pipelineReproducesInput(exhaustive.lists.uri)

    pipelineReproducesInput(exhaustive.lists.withBasics)
    pipelineReproducesInput(exhaustive.lists.withExtractors)
    pipelineReproducesInput(exhaustive.lists.withCollections)

    pipelineReproducesInput(exhaustive.lists.option)
    pipelineReproducesInput(exhaustive.lists.set)
    pipelineReproducesInput(exhaustive.lists.list)
  }

  private def pipelineReproducesInput[A : TypeKey](a: A): Unit = {
    jsonTranslator.traverse(emblemTranslator.traverse[A](a)) should equal (a)
  }

}
