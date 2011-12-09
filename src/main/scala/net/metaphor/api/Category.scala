package net.metaphor.api

trait Category[O, M] {
  def identity(o: O): M
  def compose(m1: M, m2: M): M
}

trait HeteroFunctor[O1, M1, O2, M2] {
  def source: Category[O1, M1]
  def target: Category[O2, M2]

  final def apply(o: O1): O2 = onObjects(o)
  // the dummy implicit argument is a hack to allow overloading of apply
  final def apply(m: M1)(implicit d: DummyImplicit): M2 = onMorphisms(m)
  
  def onObjects(o: O1): O2
  def onMorphisms(m: M1): M2
}

trait Functor[O, M] extends HeteroFunctor[O, M, O, M]

object Functor {
  class IdentityFunctor[O, M](category: Category[O, M]) extends Functor[O, M] {
    def source = category
    def target = category
    def onObjects(o: O) = o
    def onMorphisms(m: M) = m
  }
}

trait HeteroNaturalTransformation[O1, M1, O2, M2]  {
  def source: HeteroFunctor[O1, M1, O2, M2]
  def target: HeteroFunctor[O1, M1, O2, M2]
  def apply(o: O1): M2
}

trait NaturalTransformation[O, M] extends HeteroNaturalTransformation[O, M, O, M] {
  def source: Functor[O, M]
  def target: Functor[O, M]
}

object NaturalTransformation {
  class IdentityHeteroNaturalTransformation[O1, M1 , O2, M2 ](functor: HeteroFunctor[O1, M1, O2, M2]) extends HeteroNaturalTransformation[O1, M1, O2, M2] {
    def source = functor
    def target = functor
    def apply(o: O1) = functor.target.identity(functor(o))
  }
  class IdentityNaturalTransformation[O, M](functor: Functor[O, M]) extends NaturalTransformation[O, M] {
    def source = functor
    def target = functor
    def apply(o: O) = functor.target.identity(functor(o))
  }
}