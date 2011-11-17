package net.metaphor.api

// Morphism just exists to provide an upper bound, to disambiguate apply(object) and apply(morphism) after type erasure.
trait Morphism

trait Category[O, M <: Morphism] {
  def identity(o: O): M
  def compose(m1: M, m2: M): M
}

trait HeteroFunctor[O1, M1 <: Morphism, O2, M2 <: Morphism] extends Morphism {
  def source: Category[O1, M1]
  def target: Category[O2, M2]

  def apply(o: O1): O2
  def apply(m: M1): M2
}

trait Functor[O, M <: Morphism] extends HeteroFunctor[O, M, O, M]

object Functor {
  class IdentityFunctor[O, M <: Morphism](category: Category[O, M]) extends Functor[O, M] {
    def source = category
    def target = category
    def apply(o: O) = o
    def apply(m: M) = m
  }
}

trait HeteroNaturalTransformation[O1, M1 <: Morphism, O2, M2 <: Morphism] extends TwoMorphism {
  def source: HeteroFunctor[O1, M1, O2, M2]
  def target: HeteroFunctor[O1, M1, O2, M2]
  def apply(o: O1): M2
}

trait NaturalTransformation[O, M <: Morphism] extends HeteroNaturalTransformation[O, M, O, M] {
  def source: Functor[O, M]
  def target: Functor[O, M]
}

object NaturalTransformation {
  class IdentityHeteroNaturalTransformation[O1, M1 <: Morphism, O2, M2 <: Morphism](functor: HeteroFunctor[O1, M1, O2, M2]) extends HeteroNaturalTransformation[O1, M1, O2, M2] {
    def source = functor
    def target = functor
    def apply(o: O1) = functor.target.identity(functor(o))
  }
  class IdentityNaturalTransformation[O, M <: Morphism](functor: Functor[O, M]) extends NaturalTransformation[O, M] {
    def source = functor
    def target = functor
    def apply(o: O) = functor.target.identity(functor(o))
  }
}