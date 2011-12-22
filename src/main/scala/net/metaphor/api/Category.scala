package net.metaphor.api


trait Category[O, M, C <: Category[O, M, C]] { self: C =>
  def identity(o: O): M
  def source(m: M): O
  def target(m: M): O
  def compose(m1: M, m2: M): M

  abstract class FunctorFrom[OT, MT, CT <: Category[OT, MT, CT]] extends net.metaphor.api.HeteroFunctor[O, M, C, OT, MT, CT] {
    override val source = self
  }
  abstract class NaturalTransformationFrom[OT, MT, CT <: Category[OT, MT, CT], F <: FunctorFrom[OT, MT, CT]] extends net.metaphor.api.HeteroNaturalTransformation[O, M, C, OT, MT, CT, F] {
    override def source: F
    override def target: F
  }

  abstract class FunctorToSet extends FunctorFrom[Set, Function, Sets] with net.metaphor.api.FunctorToSet[O, M, C] 

  abstract class NaturalTransformationToSet[F <: FunctorToSet] extends NaturalTransformationFrom[Set, Function, Sets, F] with net.metaphor.api.NaturalTransformationToSet[O, M, C, F] {
    override def source: F
    override def target: F
  }

  abstract class FunctorsToSet[F <: FunctorToSet, T <: NaturalTransformationToSet[F], FC <: FunctorsToSet[F, T, FC]] extends net.metaphor.api.FunctorsToSet[O, M, C, F, T, FC](self) { functorsToSet: FC => }
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

trait HeteroNaturalTransformation[O1, M1, C1 <: Category[O1, M1, C1], O2, M2, C2 <: Category[O2, M2, C2], F <: HeteroFunctor[O1, M1, C1, O2, M2, C2]] {
  def source: F
  def target: F
  def sourceCategory = source.source
  def targetCategory = source.target
  def apply(o: O1): M2
}

trait NaturalTransformation[O, M, C <: Category[O, M, C], F <: Functor[O, M, C]] extends HeteroNaturalTransformation[O, M, C, O, M, C, F] {
  def source: F
  def target: F
}

object NaturalTransformation {
  class IdentityHeteroNaturalTransformation[O1, M1, C1 <: Category[O1, M1, C1], O2, M2, C2 <: Category[O2, M2, C2], F <: HeteroFunctor[O1, M1, C1, O2, M2, C2]](functor: F) extends HeteroNaturalTransformation[O1, M1, C1, O2, M2, C2, F] {
    def source = functor
    def target = functor
    def apply(o: O1) = functor.target.identity(functor(o))
  }
  class IdentityNaturalTransformation[O, M, C <: Category[O, M, C], F <: Functor[O, M, C]](functor: F) extends NaturalTransformation[O, M, C, F] {
    def source = functor
    def target = functor
    def apply(o: O) = functor.target.identity(functor(o))
  }
}