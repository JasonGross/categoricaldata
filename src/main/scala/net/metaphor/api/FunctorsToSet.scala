package net.metaphor.api

trait FunctorToSet extends Functor {
  override val target: Sets.type = Sets
}
trait NaturalTransformationToSet extends NaturalTransformation {
  override val source: FunctorToSet
  override val target: FunctorToSet
}
trait FunctorsToSet extends Category {
  type O <: FunctorToSet
  type M <: NaturalTransformationToSet

  override def identity(o: O): M = ???
  override def source(m: M): O = ???
  override def target(m: M): O = ???
  override def compose(m1: M, m2: M): M = ???
}

object FunctorsToSet extends FunctorsToSet {
  type O = FunctorToSet
  type M = NaturalTransformationToSet
}