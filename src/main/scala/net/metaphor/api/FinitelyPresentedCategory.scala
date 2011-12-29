package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait FinitelyPresentedCategory extends FinitelyGeneratedCategory { self =>
  def relations(source: O, target: O): List[(Path, Path)]
  def relationsFrom(source: O) = for (target <- objects; r <- relations(source, target)) yield r
  def relationsTo(target: O) = for (source <- objects; r <- relations(source, target)) yield r
  def allRelations: List[(Path, Path)] = for (source <- objects; target <- objects; r <- relations(source, target)) yield r

  // FIXME implement toString, hashcode, equals
  override def toString: String = ???
  override def hashCode: Int = ???
  override def equals(other: Any): Boolean = ???

  trait Opposite extends FinitelyPresentedCategory with super.Opposite { 
    override def relations(source: O, target: O) = self.relations(target, source)
  }
}


