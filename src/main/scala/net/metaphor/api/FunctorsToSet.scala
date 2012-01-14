package net.metaphor.api

trait FunctorToSet extends Functor {
  override val target: Sets.type = Sets
}

object FunctorToSet {
  // TODO this should just be:
  //  trait withLocallyFinitelyGeneratedSource extends FunctorToSet with Functor.withLocallyFinitelyGeneratedSource
  //  trait withFinitelyGeneratedSource extends withLocallyFinitelyGeneratedSource with Functor.withFinitelyGeneratedSource
  //  trait withFinitelyPresentedSource extends withFinitelyGeneratedSource with Functor.withFinitelyPresentedSource
  // but instead, because I'm getting AbstractMethodErrors (yay scala compiler bugs)
  trait withLocallyFinitelyGeneratedSource extends FunctorToSet {
    override val source: LocallyFinitelyGeneratedCategory
    def onGenerators(g: source.G): FFunction
    final override def onMorphisms(m: source.M) = {
      val start = onObjects(source.source(m))
      val morphisms = for (g <- m.representative.morphisms) yield onGenerators(g)
      target.compose(start, morphisms)
    }
  }
  trait withFinitelyGeneratedSource extends withLocallyFinitelyGeneratedSource {
    override val source: FinitelyGeneratedCategory
  }
  trait withFinitelyPresentedSource extends withFinitelyGeneratedSource { functor =>
    override val source: FinitelyPresentedCategory
    def verifyRelations = {
      for (relation <- source.allRelations) {
        require(functor.onMorphisms(source.pathAsMorphism(relation._1)) == functor.onMorphisms(source.pathAsMorphism(relation._2)))
      }
    }

  }
}

trait NaturalTransformationToSet extends NaturalTransformation {
  override val source: FunctorToSet
  override val target: FunctorToSet
}
trait FunctorsToSet extends Category {
  type O <: FunctorToSet
  type M <: NaturalTransformationToSet

  def internalize(f: net.metaphor.api.FunctorToSet): O
  def internalize(t: net.metaphor.api.NaturalTransformationToSet): M

  override def identity(o: O) = internalize(???)
  override def source(m: M) = internalize(m.source)
  override def target(m: M) = internalize(m.target)
  override def compose(m1: M, m2: M) = internalize(???)

}

object FunctorsToSet extends FunctorsToSet {
  type O = FunctorToSet
  type M = NaturalTransformationToSet

  override def internalize(o: O) = o
  override def internalize(m: M) = m
}