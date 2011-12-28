package net.metaphor.api

trait SmallFunctor extends Functor { functor =>
  override val source: SmallCategory
  override val target: SmallCategory

  trait ContravariantDataFunctor extends Functor {
    override val source = functor.target.AllFunctorsToSet
    override val target: functor.source.SpecializedFunctorsToSet = functor.source.functorsToSet

//        override def apply(i: SmallCategory#FunctorToSet) = super.apply(functor.target.internalize(i))
//        def apply(m: net.metaphor.api.NaturalTransformationToSet) = {
//          super.apply(functor.target.internalize(m))
//        }
  }
  trait CovariantDataFunctor extends Functor {
    override val source = functor.source.AllFunctorsToSet
    override val target: functor.target.SpecializedFunctorsToSet = functor.target.functorsToSet

//        def apply(i: SmallCategory#FunctorToSet) = super.apply(functor.source.internalize(i))
//        def apply(m: SmallCategory#NaturalTransformationToSet) = {
//          super.apply(functor.source.internalize(m))
//        }
  }

  object Pullback extends ContravariantDataFunctor {
    override def onObjects(i: functor.target.FunctorToSet) = functor.source.internalize(new functor.source.FunctorToSet {
      def onObjects(o: functor.source.O) = i(functor.apply(o))
      def onMorphisms(m: functor.source.M) = i(functor.apply(m))
    })
    override def onMorphisms(m: functor.target.NaturalTransformationToSet) = functor.source.internalize(new functor.source.NaturalTransformationToSet {
      val source = onObjects(m.target)
      val target = onObjects(m.source)
      def apply(o: functor.source.O) = m(functor.apply(o))
    })
  }

//  object Pullback extends Pullback
  
  def ^* = new Functor {
    override val source = FunctorsToSet 
    override val target = functor.source.functorsToSet
    def onObjects(i: FunctorToSet) = Pullback.apply(functor.target.internalize(i))
    def onMorphisms(t: NaturalTransformationToSet) = Pullback.apply(functor.target.internalize(t))
  } 
}
