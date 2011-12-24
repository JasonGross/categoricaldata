package net.metaphor.api

trait HeteroFunctor[O1, M1, C1 <: Category[O1, M1, C1], O2, M2, C2 <: Category[O2, M2, C2]] {
  def source: C1
  def target: C2

  final def apply(o: O1): O2 = onObjects(o)
  // the dummy implicit argument is a hack to allow overloading of apply
  final def apply(m: M1)(implicit d: DummyImplicit): M2 = onMorphisms(m)

  def onObjects(o: O1): O2
  def onMorphisms(m: M1): M2
}

trait SmallHeteroFunctor[O1, M1, C1 <: SmallCategory[O1, M1, C1], O2, M2, C2 <: SmallCategory[O2, M2, C2]] extends HeteroFunctor[O1, M1, C1, O2, M2, C2]{ functor =>
  val source: C1
  val target: C2

  trait ContravariantDataFunctor extends HeteroFunctor[target.F, target.T, target.CSets, source.F, source.T, source.CSets] {
    val source = functor.target.functorsToSet
    val target = functor.source.functorsToSet

    def apply(i: C2#F) = super.apply(functor.target.liftFunctorToSet(i))
    def apply(m: C2#T) = {
      // FIXME this will almost certainly break at runtime. :-(
      super.apply(functor.target.liftNaturalTransformationToSet(m.asInstanceOf[net.metaphor.api.NaturalTransformationToSet[O2, M2, C2, functor.target.F]]))
    }    
  }
  trait CovariantDataFunctor extends HeteroFunctor[source.F, source.T, source.CSets,target.F, target.T, target.CSets] {
    val source = functor.source.functorsToSet
    val target = functor.target.functorsToSet

    def apply(i: C1#F) = super.apply(functor.source.liftFunctorToSet(i))
    def apply(m: C1#T) = {
      // FIXME this will almost certainly break at runtime. :-(
      super.apply(functor.source.liftNaturalTransformationToSet(m.asInstanceOf[net.metaphor.api.NaturalTransformationToSet[O1, M1, C1, functor.source.F]]))
    }    
  }

  trait Pullback extends ContravariantDataFunctor {
    def onObjects(i: functor.target.F) = functor.source.liftFunctorToSet(new functor.source.FunctorToSet {
      def onObjects(o: O1) = i(functor(o))
      def onMorphisms(m: M1) = i(functor(m))
    })
    def onMorphisms(m: functor.target.T) = functor.source.liftNaturalTransformationToSet(new functor.source.NaturalTransformationToSet[functor.source.F] {
      def source = onObjects(m.source)
      def target = onObjects(m.target)
      def apply(o: O1) = m(functor(o))
    })
  }

  def pullback: Pullback = new Pullback {}

  def ^* = pullback

}

// TODO consider, throwing out these traits, or at least demoting them to types.

trait Functor[O, M, C <: Category[O, M, C]] extends HeteroFunctor[O, M, C, O, M, C] { functor => }

trait SmallFunctor[O, M, C <: SmallCategory[O, M, C]] extends Functor[O, M, C] with SmallHeteroFunctor[O, M, C, O, M, C] { functor => }

object Functor {
  class IdentityFunctor[O, M, C <: Category[O, M, C]](category: C) extends Functor[O, M, C] {
    val source = category
    val target = category
    def onObjects(o: O) = o
    def onMorphisms(m: M) = m
  }

  class CompositeFunctor[O, M, C <: Category[O, M, C]](functor1: Functor[O, M, C], functor2: Functor[O, M, C]) extends Functor[O, M, C] {
    val source = functor1.source
    val target = functor2.source
    def onObjects(o: O) = functor2(functor1(o))
    def onMorphisms(m: M) = functor2(functor1(m))

  }
}
