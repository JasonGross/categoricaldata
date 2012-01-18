package net.categoricaldata.category

import net.categoricaldata.sets._

trait FunctorToSet extends Functor {
  override val target: Sets.type = Sets  
}
trait NaturalTransformationToSet extends NaturalTransformation {
  override val source: FunctorToSet
  override val target: FunctorToSet
  
  def isomorphism_? : Boolean = ???
}
trait FunctorsToSet extends Category {
  type O <: FunctorToSet
  type M <: NaturalTransformationToSet

  def internalize(f: net.categoricaldata.category.FunctorToSet): O
  def internalize(t: net.categoricaldata.category.NaturalTransformationToSet): M

  override def identity(o: O) = internalize(???)
  override def source(m: M) = internalize(m.source)
  override def target(m: M) = internalize(m.target)
  override def compose(m1: M, m2: M) = internalize(???)
  
}

object FunctorsToSet extends FunctorsToSet {
  type O = FunctorToSet
  type M = NaturalTransformationToSet

  override def internalize(o: O) = o
  override def internalize(m: M) = m
}