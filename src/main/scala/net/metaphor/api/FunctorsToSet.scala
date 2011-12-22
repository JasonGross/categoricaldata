package net.metaphor.api

trait FunctorToSet[O, M, C <: Category[O, M, C]] extends HeteroFunctor[O, M, C, Set, Function, Sets] {
  override val target = Sets
}
trait NaturalTransformationToSet[O, M, C <: Category[O, M, C], F <: FunctorToSet[O, M, C]] extends HeteroNaturalTransformation[O, M, C, Set, Function, Sets, F] {
  override def source: F
  override def target: F
}

abstract class FunctorsToSet[O, M, C <: Category[O, M, C], F <: C#FunctorToSet, T <: C#NaturalTransformationToSet[F], FC <: FunctorsToSet[O, M, C, F, T, FC]](source: C) extends FunctorCategory[O, M, C, Set, Function, Sets, F, T, FC](source, Sets) { self: FC => }