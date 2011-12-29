package net.metaphor.api

trait SmallFunctor extends Functor { smallFunctor =>
  override val source: SmallCategory
  override val target: SmallCategory

  trait ContravariantDataFunctor extends Functor {
    override val source = smallFunctor.target.AllFunctorsToSet
    override val target: smallFunctor.source.SpecializedFunctorsToSet = smallFunctor.source.functorsToSet
//        override def apply(i: SmallCategory#FunctorToSet) = super.apply(functor.target.internalize(i))
//        def apply(m: net.metaphor.api.NaturalTransformationToSet) = {
//          super.apply(functor.target.internalize(m))
//        }
  }
  trait CovariantDataFunctor extends Functor {
    override val source = smallFunctor.source.AllFunctorsToSet
    override val target: smallFunctor.target.SpecializedFunctorsToSet = smallFunctor.target.functorsToSet
//        def apply(i: SmallCategory#FunctorToSet) = super.apply(functor.source.internalize(i))
//        def apply(m: SmallCategory#NaturalTransformationToSet) = {
//          super.apply(functor.source.internalize(m))
//        }
  }

  trait Pullback extends ContravariantDataFunctor {
    override def onObjects(i: smallFunctor.target.FunctorToSet) = smallFunctor.source.internalize(new smallFunctor.source.FunctorToSet {
      def onObjects(o: smallFunctor.source.O) = i(smallFunctor.apply(o))
      def onMorphisms(m: smallFunctor.source.M) = i(smallFunctor.apply(m))
    })
    override def onMorphisms(m: smallFunctor.target.NaturalTransformationToSet) = smallFunctor.source.internalize(new smallFunctor.source.NaturalTransformationToSet {
      val source = onObjects(m.target)
      val target = onObjects(m.source)
      def apply(o: smallFunctor.source.O) = m(smallFunctor.apply(o))
    })
  }

  lazy val pullback = new Pullback { }
  
//  object Pullback extends Pullback
  
  lazy val ^* = new Functor {
    override val source = FunctorsToSet 
    override val target = smallFunctor.source.functorsToSet
    def onObjects(i: FunctorToSet) = pullback.apply(smallFunctor.target.internalize(i))
    def onMorphisms(t: NaturalTransformationToSet) = pullback.apply(smallFunctor.target.internalize(t))
  } 
}
