package net.metaphor.api

trait FunctorToSet extends Functor {
  override val target: Sets.type = Sets
}
trait NaturalTransformationToSet extends NaturalTransformation {
  override val targetCategory: Sets.type = Sets
  override val source: FunctorToSet
  override val target: FunctorToSet
}

//abstract class FunctorsToSet(source: Category) extends FunctorCategory(source, Sets) { self =>
//  override type O <: Category#FunctorToSet
//  override type M <: Category#NaturalTransformationToSet
//}