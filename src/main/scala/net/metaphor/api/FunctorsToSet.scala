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

  def internalize(f: net.metaphor.api.FunctorToSet): O
  def internalize(t: net.metaphor.api.NaturalTransformationToSet): M

  override def identity(o: O) = internalize(???)
  override def source(m: M) = internalize(m.source)
  override def target(m: M) = internalize(m.target)
  override def compose(m1: M, m2: M) = internalize(???)
  
//  override lazy val opposite = new Opposite { }
}

object FunctorsToSet extends FunctorsToSet {
  type O = FunctorToSet
  type M = NaturalTransformationToSet

  override def internalize(o: O) = o
  override def internalize(m: M) = m
}