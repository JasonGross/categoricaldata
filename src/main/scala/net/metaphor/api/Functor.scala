package net.metaphor.api

trait Functor {
  val source: Category
  val target: Category

  final def apply(o: source.O): target.O = onObjects(o)
  // the dummy implicit argument is a hack to allow overloading of apply
  final def apply(m: source.M)(implicit d: DummyImplicit): target.M = onMorphisms(m)

  def onObjects(o: source.O): target.O
  def onMorphisms(m: source.M): target.M
}

trait SmallFunctor extends Functor { functor =>
  override val source: SmallCategory
  override val target: SmallCategory
  
  trait ContravariantDataFunctor extends Functor {
    override val source: functor.target.FunctorsToSet = functor.target.functorsToSet
    override val target: functor.source.FunctorsToSet = functor.source.functorsToSet

    def apply(i: SmallCategory#FunctorToSet) = super.apply(functor.target.internalize(i))
    def apply(m: SmallCategory#NaturalTransformationToSet) = {
      super.apply(functor.target.internalize(m))
    }
  }
  trait CovariantDataFunctor extends Functor {
    val source: functor.source.FunctorsToSet = functor.source.functorsToSet
    val target: functor.target.FunctorsToSet = functor.target.functorsToSet

    def apply(i: SmallCategory#FunctorToSet) = super.apply(functor.source.internalize(i))
    def apply(m: SmallCategory#NaturalTransformationToSet) = {
      super.apply(functor.source.internalize(m))
    }
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

object Functor {
  class IdentityFunctor(val category: Category) extends Functor {
    val source: category.type = category
    val target: category.type = category
    def onObjects(o: category.O) = o
    def onMorphisms(m: category.M) = m
  }

  class CompositeFunctor(val functor1: Functor, val functor2: Functor) extends Functor {
    require(functor1.target == functor2.source)
    val source: functor1.source.type = functor1.source
    val target: functor2.target.type = functor2.target
    def onObjects(o: source.O) = functor2(functor1(o).asInstanceOf[functor2.source.O])
    def onMorphisms(m: source.M) = functor2(functor1(m).asInstanceOf[functor2.source.M])

  }
}
