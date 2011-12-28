package net.metaphor.api

trait FunctorToSet extends Functor {
  override val target: Sets.type = Sets
}
trait NaturalTransformationToSet extends NaturalTransformation {
  override val targetCategory: Sets.type = Sets
  override val source: FunctorToSet
  override val target: FunctorToSet
}
