package net.metaphor.api


trait Category[O, M, C <: Category[O, M, C]] { self: C =>
  def identity(o: O): M
  def source(m: M): O
  def target(m: M): O
  def compose(m1: M, m2: M): M

  abstract class FunctorFrom[OT, MT, CT <: Category[OT, MT, CT]] extends net.metaphor.api.HeteroFunctor[O, M, C, OT, MT, CT] {
    override val source = self
  }
  abstract class NaturalTransformationFrom[OT, MT, CT <: Category[OT, MT, CT]] extends net.metaphor.api.HeteroNaturalTransformation[O, M, C, OT, MT, CT] {
    override def source: FunctorFrom[OT, MT, CT]
    override def target: FunctorFrom[OT, MT, CT]
  }

  abstract class FunctorToSet extends FunctorFrom[Set, Function, Sets] with net.metaphor.api.FunctorToSet[O, M, C] 

  abstract class NaturalTransformationToSet extends NaturalTransformationFrom[Set, Function, Sets] with net.metaphor.api.NaturalTransformationToSet[O, M, C] {
    override def source: FunctorToSet
    override def target: FunctorToSet
  }

  class FunctorsToSet extends net.metaphor.api.FunctorsToSet[O, M, C](self)
}

trait HeteroFunctor[O1, M1, C1 <: Category[O1, M1, C1], O2, M2, C2 <: Category[O2, M2, C2]] {
  def source: C1
  def target: C2

  final def apply(o: O1): O2 = onObjects(o)
  // the dummy implicit argument is a hack to allow overloading of apply
  final def apply(m: M1)(implicit d: DummyImplicit): M2 = onMorphisms(m)

  def onObjects(o: O1): O2
  def onMorphisms(m: M1): M2
}

trait Functor[O, M, C <: Category[O, M, C]] extends HeteroFunctor[O, M, C, O, M, C]

object Functor {
  class IdentityFunctor[O, M, C <: Category[O, M, C]](category: C) extends Functor[O, M, C] {
    def source = category
    def target = category
    def onObjects(o: O) = o
    def onMorphisms(m: M) = m
  }
}

trait HeteroNaturalTransformation[O1, M1, C1 <: Category[O1, M1, C1], O2, M2, C2 <: Category[O2, M2, C2]] {
  def source: HeteroFunctor[O1, M1, C1, O2, M2, C2]
  def target: HeteroFunctor[O1, M1, C1, O2, M2, C2]
  def apply(o: O1): M2
}

trait NaturalTransformation[O, M, C <: Category[O, M, C]] extends HeteroNaturalTransformation[O, M, C, O, M, C] {
  def source: Functor[O, M, C]
  def target: Functor[O, M, C]
}

object NaturalTransformation {
  class IdentityHeteroNaturalTransformation[O1, M1, C1 <: Category[O1, M1, C1], O2, M2, C2 <: Category[O2, M2, C2]](functor: HeteroFunctor[O1, M1, C1, O2, M2, C2]) extends HeteroNaturalTransformation[O1, M1, C1, O2, M2, C2] {
    def source = functor
    def target = functor
    def apply(o: O1) = functor.target.identity(functor(o))
  }
  class IdentityNaturalTransformation[O, M, C <: Category[O, M, C]](functor: Functor[O, M, C]) extends NaturalTransformation[O, M, C] {
    def source = functor
    def target = functor
    def apply(o: O) = functor.target.identity(functor(o))
  }
}