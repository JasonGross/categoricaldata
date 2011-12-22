package net.metaphor.api

trait Category[O, M, C <: Category[O, M, C]] { self: C =>
  def identity(o: O): M
  def source(m: M): O
  def target(m: M): O
  def compose(m1: M, m2: M): M

  abstract class FunctorFrom[OT, MT, CT <: Category[OT, MT, CT]] extends HeteroFunctor[O, M, C, OT, MT, CT] {
    override val source = self
  }
  abstract class NaturalTransformationFrom[OT, MT, CT <: Category[OT, MT, CT], F <: FunctorFrom[OT, MT, CT]] extends HeteroNaturalTransformation[O, M, C, OT, MT, CT, F] {
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