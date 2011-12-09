package net.metaphor.api

trait TwoCategory[M0, M1, M2] {
  def identity0(m0: M0): M1
  def identity1(m1: M1): M2

  def compose(a1: M1, b1: M1): M1
  def compose1(a2: M2, b2: M2): M2
  def compose2(a2: M2, b2: M2): M2

  def whiskerRight(a2: M2, b1: M1) = compose1(a2, identity1(b1))
  def whiskerLeft(a1: M1, b2: M2) = compose1(identity1(a1), b2)
}

trait HeteroTwoFunctor[S0, S1, S2, T0, T1, T2] {
  def source: TwoCategory[S0, S1, S2]
  def target: TwoCategory[T0, T1, T2]
  
  final def apply(m0: S0): T0 = onZeroMorphisms(m0)
  // the dummy implicit arguments is a hack to allow overloading of apply
  final def apply(m1: S1)(implicit d: DummyImplicit): T1 = onOneMorphisms(m1)
  final def apply(m2: S2)(implicit d1: DummyImplicit, d2: DummyImplicit): T2 = onTwoMorphisms(m2)
  
  def onZeroMorphisms(m0: S0): T0
  def onOneMorphisms(m1: S1): T1
  def onTwoMorphisms(m2: S2): T2
}

object Categories {
  class CompositeHeteroFunctor[O1, M1, O2, M2, O3, M3](first: HeteroFunctor[O1, M1, O2, M2], second: HeteroFunctor[O2, M2, O3, M3]) extends HeteroFunctor[O1, M1, O3, M3] {
    def source = first.source
    def target = second.target

    def onObjects(o: O1) = second(first(o))
    def onMorphisms(m: M1) = second(first(m))
  }
  class CompositeFunctor[O, M](first: Functor[O, M], second: Functor[O, M]) extends Functor[O, M] {
    def source = first.source
    def target = second.target

    def onObjects(o: O) = second(first(o))
    def onMorphisms(m: M) = second(first(m))
  }
}

trait Categories[O, M, C <: Category[O, M]] extends TwoCategory[C, Functor[O, M], NaturalTransformation[O, M]] {
  def identity0(category: C) = new Functor.IdentityFunctor[O, M](category)
  def identity1(functor: Functor[O, M]) = new NaturalTransformation.IdentityNaturalTransformation(functor)

  def compose(first: Functor[O, M], second: Functor[O, M]) = new Categories.CompositeFunctor(first, second)
  def compose1(first: NaturalTransformation[O, M], second: NaturalTransformation[O, M]) = new Composite1NaturalTransformation(first, second)
  def compose2(first: NaturalTransformation[O, M], second: NaturalTransformation[O, M]) = new Composite2NaturalTransformation(first, second)

  class Composite1NaturalTransformation(first: NaturalTransformation[O, M], second: NaturalTransformation[O, M]) extends NaturalTransformation[O, M] {
    def source = compose(first.source, second.source)
    def target = compose(first.target, second.target)

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

trait RigidTwoCategory[M0, M1, M2] extends TwoCategory[M0, M1, M2] {
  def leftDual(m1: M1): M1
  def leftPairing(m1: M1): M2
  def leftCopairing(m1: M1): M2
  def rotateLeft(m2: M2): M2

  def rightDual(m1: M1): M1
  def rightPairing(m1: M1): M2
  def rightCopairing(m1: M1): M2
  def rotateRight(m2: M2): M2
}