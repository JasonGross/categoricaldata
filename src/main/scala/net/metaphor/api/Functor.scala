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

trait Functor[O, M, C <: Category[O, M, C]] extends HeteroFunctor[O, M, C, O, M, C] { functor => }

trait SmallFunctor[O, M, C <: SmallCategory[O, M, C]] extends Functor[O, M, C] {functor =>
  val source: C
  val target: C
  
  trait ContravariantDataFunctor extends HeteroFunctor[target.F, target.T, target.CSets, source.F, source.T, source.CSets] {
    val source = functor.target.functorsToSet
    val target = functor.source.functorsToSet
    
    // TODO convenience methods for widening the scope.
  }
  
    trait Pullback extends ContravariantDataFunctor

//  def pullback: Pullback = new Pullback {
//    def onObjects(i: functor.target.F) = functor.source.lift(new functor.source.FunctorToSet {
//      def onObjects(o: O) = i(functor(o))
//      def onMorphisms(m: M) = i(functor(m))
//    })
//    def onMorphisms(m: functor.target.T) = functor.source.lift(new functor.source.NaturalTransformationToSet[functor.source.F] {
//      def source = onObjects(m.source)
//      def target = onObjects(m.target)
//      def apply(o: O) = m(functor(o))
//    })
//  }  
}

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
