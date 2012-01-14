package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait FinitelyPresentedCategory extends FinitelyGeneratedCategory { fpCategory =>
  def relations(source: O, target: O): List[(Path, Path)]
  def relationsFrom(source: O) = for (target <- objects; r <- relations(source, target)) yield r
  def relationsTo(target: O) = for (source <- objects; r <- relations(source, target)) yield r
  def allRelations: List[(Path, Path)] = for (source <- objects; target <- objects; r <- relations(source, target)) yield r

  private def symmetrizedRelations = allRelations.map({ r => Set(r, r.swap) })

  override def equals(other: Any) = {
    other match {
      case other: FinitelyPresentedCategory => {
        objects.toSet == other.objects.toSet && allGenerators.toSet == other.allGenerators.toSet && symmetrizedRelations.toSet == other.symmetrizedRelations.toSet
      }
      case _ => false
    }
  }

  override def hashCode = {
    (objects.toSet, allGenerators.toSet, symmetrizedRelations.toSet).hashCode
  }

  def findIsomorphismsTo(other: FinitelyPresentedCategory): Iterable[Functor] = ???
  def isIsomorphicTo(other: FinitelyPresentedCategory) = findIsomorphismsTo(other).nonEmpty

  def findEquivalencesTo(other: FinitelyPresentedCategory): Iterable[Equivalence] = ???
  def isEquivalentTo(other: FinitelyPresentedCategory) = findEquivalencesTo(other).nonEmpty

  trait OppositeFinitelyPresentedCategory extends FinitelyPresentedCategory with OppositeFinitelyGeneratedCategory {
    override def relations(source: O, target: O) = for ((Path(_, _, g1s), Path(_, _, g2s)) <- fpCategory.relations(target, source)) yield {
      (Path(source, target, g1s.reverse.map(reverseGenerator(_))), Path(source, target, g2s.reverse.map(reverseGenerator(_))))
    }
  }

  trait Wrapper extends super.Wrapper with FinitelyPresentedCategory {
    override def relations(s: O, t: O) = fpCategory.relations(s, t)
  }

  abstract class FullSubcategory(spannedBy: List[O]) extends super.FullSubcategory(spannedBy) with FinitelyPresentedCategory {
    override def relations(s: O, t: O) = fpCategory.relations(s, t)
  }

  class ConcreteFullSubcategory(spannedBy: List[O]) extends FullSubcategory(spannedBy) with FinitelyGeneratedCategories.StandardFunctorsToSet

  class FullSubcategoryInclusion(spannedBy: List[O]) extends super.FullSubcategoryInclusion(spannedBy) with Functor.withFinitelyPresentedSource.withFinitelyPresentedTarget {
    override val source: FullSubcategory = new ConcreteFullSubcategory(spannedBy)
  }

  override def fullSubcategoryInclusion(spannedBy: List[O]): FullSubcategoryInclusion = new FullSubcategoryInclusion(spannedBy)
  override def fullSubcategory(spannedBy: List[O]): FullSubcategory = fullSubcategoryInclusion(spannedBy).source

  trait FunctorToSet extends super.FunctorToSet with FunctorToSet.withFinitelyPresentedSource
}


