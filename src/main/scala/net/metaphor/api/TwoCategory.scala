package net.metaphor.api

trait TwoMorphism

trait TwoCategory[M0, M1 <: Morphism, M2 <: TwoMorphism] {
  def identity0(m0: M0): M1
  def identity1(m1: M1): M2

  def compose1(a1: M1, b1: M1): M1
  def compose1(a2: M2, b2: M2): M2
  def compose2(a2: M2, b2: M2): M2

  def whisker(a2: M2, b1: M1) = compose1(a2, identity1(b1))
  def whisker(a1: M1, b2: M2) = compose1(identity1(a1), b2)
}

trait HeteroTwoFunctor[S0, S1 <: Morphism, S2 <: TwoMorphism, T0, T1 <: Morphism, T2 <: TwoMorphism] {
  def source: TwoCategory[S0, S1, S2]
  def target: TwoCategory[T0, T1, T2]
  
  def apply(m0: S0): T0
  def apply(m1: S1): T1
  def apply(m2: S2): T2
}

object Categories {
  class CompositeHeteroFunctor[O1, M1 <: Morphism, O2, M2 <: Morphism, O3, M3 <: Morphism](first: HeteroFunctor[O1, M1, O2, M2], second: HeteroFunctor[O2, M2, O3, M3]) extends HeteroFunctor[O1, M1, O3, M3] {
    def source = first.source
    def target = second.target

    def apply(o: O1) = second(first(o))
    def apply(m: M1) = second(first(m))
  }
  class CompositeFunctor[O, M <: Morphism](first: Functor[O, M], second: Functor[O, M]) extends Functor[O, M] {
    def source = first.source
    def target = second.target

    def apply(o: O) = second(first(o))
    def apply(m: M) = second(first(m))
  }
}

trait Categories[O, M <: Morphism, C <: Category[O, M]] extends TwoCategory[C, Functor[O, M], NaturalTransformation[O, M]] {
  def identity0(category: C) = new Functor.IdentityFunctor[O, M](category)
  def identity1(functor: Functor[O, M]) = new NaturalTransformation.IdentityNaturalTransformation(functor)

  def compose1(first: Functor[O, M], second: Functor[O, M]) = new Categories.CompositeFunctor(first, second)
  def compose1(first: NaturalTransformation[O, M], second: NaturalTransformation[O, M]) = new Composite1NaturalTransformation(first, second)
  def compose2(first: NaturalTransformation[O, M], second: NaturalTransformation[O, M]) = new Composite2NaturalTransformation(first, second)

  class Composite1NaturalTransformation(first: NaturalTransformation[O, M], second: NaturalTransformation[O, M]) extends NaturalTransformation[O, M] {
    def source = compose1(first.source, second.source)
    def target = compose1(first.target, second.target)

    /** 
     * F(G(x)) ---> F(G'(x)) ---> F'(G'(x))
     */
    def apply(o: O) = target.target.compose(second.source(first(o)), first(second.target(o)))
  }
  class Composite2NaturalTransformation(first: NaturalTransformation[O, M], second: NaturalTransformation[O, M]) extends NaturalTransformation[O, M] {
    def source = first.source
    def target = second.target

    def apply(o: O) = target.target.compose(first(o), second(o))
  }
}

trait RigidTwoCategory[M0, M1 <: Morphism, M2 <: TwoMorphism] extends TwoCategory[M0, M1, M2] {
  def leftDual(m1: M1): M1
  def leftPairing(m1: M1): M2
  def leftCopairing(m1: M1): M2
  def rotateLeft(m2: M2): M2

  def rightDual(m1: M1): M1
  def rightPairing(m1: M1): M2
  def rightCopairing(m1: M1): M2
  def rotateRight(m2: M2): M2
}