package net.metaphor.api

trait SmallFunctor extends Functor { functor =>
  override val source: SmallCategory
  override val target: SmallCategory
  
  trait ContravariantDataFunctor extends Functor {
    override val source: functor.target.FunctorsToSet = functor.target.functorsToSet
    override val target: functor.source.FunctorsToSet = functor.source.functorsToSet

//    def apply(i: SmallCategory#FunctorToSet) = super.apply(functor.target.internalize(i))
//    def apply(m: SmallCategory#NaturalTransformationToSet) = {
//      super.apply(functor.target.internalize(m))
//    }
  }
  trait CovariantDataFunctor extends Functor {
    val source: functor.source.FunctorsToSet = functor.source.functorsToSet
    val target: functor.target.FunctorsToSet = functor.target.functorsToSet

//    def apply(i: SmallCategory#FunctorToSet) = super.apply(functor.source.internalize(i))
//    def apply(m: SmallCategory#NaturalTransformationToSet) = {
//      super.apply(functor.source.internalize(m))
//    }
  }

  trait Pullback extends ContravariantDataFunctor {
    def onObjects(i: functor.target.F) = functor.source.internalize(new functor.source.FunctorToSet {
      def onObjects(o: functor.source.O) = i(functor.apply(o))
      def onMorphisms(m: functor.source.M) = i(functor.apply(m))
    })
    def onMorphisms(m: functor.target.T) = functor.source.internalize(new functor.source.NaturalTransformationToSet {
      val source = onObjects(m.target)
      val target = onObjects(m.source)
      def apply(o: functor.source.O) = m(functor.apply(o))
    })
  }

  def pullback: Pullback = new Pullback {}

  def ^* = pullback

}
