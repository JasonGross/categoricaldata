package net.metaphor.api

trait FunctorToSet[C <: Category[C]] extends HeteroFunctor[C, Sets] {
  override val target: Sets.type = Sets
}
trait NaturalTransformationToSet[C <: Category[C], F <: FunctorToSet[C]] extends HeteroNaturalTransformation[C, Sets, F] {
  override val targetCategory: Sets.type = Sets
  override val source: F
  override val target: F
}

abstract class FunctorsToSet[C <: Category[C], FC <: FunctorsToSet[C, FC]](source: C) extends FunctorCategory[C, Sets, FC](source, Sets) { self: FC =>
  override type O <: C#FunctorToSet
  override type M <: C#NaturalTransformationToSet[O]
}