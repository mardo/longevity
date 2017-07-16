package longevity.test

import longevity.model.PEv

trait TestDataGeneratorExperiment[M] {

  def generate[P : PEv[M, ?]]: P

}

private[longevity] object TestDataGeneratorExperiment {

  import org.scalacheck.Arbitrary
  import org.scalacheck.rng.Seed
  import org.scalacheck.Gen
  import org.scalacheck.derive.MkHListArbitrary
  import shapeless.Generic
  import shapeless.HList

  def apply[M] = new TestDataGeneratorExperiment[M] {
    def generate[P : PEv[M, ?]]: P = {

      // TODO this is entirely experimental and could probably be thrown aside by now
      // TODO next step is change the TDG API on master, merge it up to here

      // TODO:
      // - put the Arbitrary[P] in the PEv. then you won't have to build it from pieces, you can just say this
      //   inside the macro expansion for `object P`:
      // 
      //   import org.scalacheck.ScalacheckShapeless._
      //   implicitly[Arbitrary[P]]

      val gen = {
        import org.scalacheck.ScalacheckShapeless._
        type G[P] = Generic[P] {
          type Repr <: HList
        }
        implicit val gen: G[P] = ???
        type AUX = Generic.Aux[P, gen.Repr]

        //implicit val aux: AUX = shapeless.Generic.materialize[P, gen.Repr] // error: not a case class
        implicit val aux: AUX = ???
        implicit val mkarb: MkHListArbitrary[gen.Repr] = ???
        implicitly[Arbitrary[P]].arbitrary
      }

      val seed = Seed.apply(System.currentTimeMillis)
      gen.pureApply(Gen.Parameters.default, seed)

    }
  }
}
