package net.metaphor.api

trait HeteroFunctor[C1 <: Category[C1], C2 <: Category[C2]] {
  val source: C1
  val target: C2

  final def apply(o: source.O): target.O = onObjects(o)
  // the dummy implicit argument is a hack to allow overloading of apply
  final def apply(m: source.M)(implicit d: DummyImplicit): target.M = onMorphisms(m)

  def onObjects(o: source.O): target.O
  def onMorphisms(m: source.M): target.M
}

trait SmallHeteroFunctor[C1 <: SmallCategory[C1], C2 <: SmallCategory[C2]] extends HeteroFunctor[C1, C2]{ functor =>
  trait ContravariantDataFunctor extends HeteroFunctor[target.CSets, source.CSets] {
    val source = functor.target.functorsToSet
    val target = functor.source.functorsToSet

    def apply(i: C2#F) = super.apply(functor.target.liftFunctorToSet(i))
    def apply(m: C2#T) = {
      // FIXME this will almost certainly break at runtime. :-(
      super.apply(functor.target.liftNaturalTransformationToSet(m.asInstanceOf[net.metaphor.api.NaturalTransformationToSet[C2, functor.target.F]]))
    }    
  }
  trait CovariantDataFunctor extends HeteroFunctor[source.CSets, target.CSets] {
    val source = functor.source.functorsToSet
    val target = functor.target.functorsToSet

    def apply(i: C1#F) = super.apply(functor.source.liftFunctorToSet(i))
    def apply(m: C1#T) = {
      // FIXME this will almost certainly break at runtime. :-(
      super.apply(functor.source.liftNaturalTransformationToSet(m.asInstanceOf[net.metaphor.api.NaturalTransformationToSet[C1, functor.source.F]]))
    }    
  }

  trait Pullback extends ContravariantDataFunctor {
    def onObjects(i: functor.target.F) = functor.source.liftFunctorToSet(new functor.source.FunctorToSet {
      def onObjects(o: functor.source.O) = i(functor.apply(o))
      def onMorphisms(m: functor.source.M) = i(functor.apply(m))
    })
    def onMorphisms(m: functor.target.T) = functor.source.liftNaturalTransformationToSet(new functor.source.NaturalTransformationToSet[functor.source.F] {
      val source = onObjects(m.target)
      val target = onObjects(m.source)
      def apply(o: functor.source.O) = m(functor.apply(o).asInstanceOf[m.sourceCategory.O])
    })
  }

  def pullback: Pullback = new Pullback {}

  def ^* = pullback

}

// TODO consider, throwing out these traits, or at least demoting them to types.

trait Functor[C <: Category[C]] extends HeteroFunctor[C, C] { functor => }

trait SmallFunctor[C <: SmallCategory[C]] extends Functor[C] with SmallHeteroFunctor[C, C] { functor => }

object Functor {
  class IdentityFunctor[C <: Category[C]](val category: C) extends Functor[C] {
    val source: category.type = category
    val target: category.type = category
    def onObjects(o: category.O) = o
    def onMorphisms(m: category.M) = m
  }

  class CompositeFunctor[C <: Category[C]](val functor1: Functor[C], val functor2: Functor[C]) extends Functor[C] {
    require(functor1.target == functor2.source)
    val source: functor1.source.type = functor1.source
    val target: functor2.target.type = functor2.target
    def onObjects(o: source.O) = functor2(functor1(o).asInstanceOf[functor2.source.O])
    def onMorphisms(m: source.M) = functor2(functor1(m).asInstanceOf[functor2.source.M])

  }
}
