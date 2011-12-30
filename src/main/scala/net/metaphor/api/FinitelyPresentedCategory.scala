package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait FinitelyPresentedCategory extends FinitelyGeneratedCategory { fpCategory =>
  def relations(source: O, target: O): List[(Path, Path)]
  def relationsFrom(source: O) = for (target <- objects; r <- relations(source, target)) yield r
  def relationsTo(target: O) = for (source <- objects; r <- relations(source, target)) yield r
  def allRelations: List[(Path, Path)] = for (source <- objects; target <- objects; r <- relations(source, target)) yield r

  // FIXME implement toString, hashcode, equals
  override def toString: String = ???
  override def hashCode: Int = ???
  override def equals(other: Any): Boolean = ???

  trait Opposite extends FinitelyPresentedCategory with super.Opposite { 
    override def relations(source: O, target: O) = for((Path(_, _, g1s), Path(_, _, g2s)) <- fpCategory.relations(target, source)) yield {
      (Path(source, target, g1s.reverse.map(reverse(_))), Path(source, target, g2s.reverse.map(reverse(_))))
    }
  }
}


