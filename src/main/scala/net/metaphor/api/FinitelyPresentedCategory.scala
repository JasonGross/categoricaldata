package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait FinitelyPresentedCategory extends FinitelyGeneratedCategory { fpCategory =>
  def relations(source: O, target: O): List[(Path, Path)]
  def relationsFrom(source: O) = for (target <- objects; r <- relations(source, target)) yield r
  def relationsTo(target: O) = for (source <- objects; r <- relations(source, target)) yield r
  def allRelations: List[(Path, Path)] = for (source <- objects; target <- objects; r <- relations(source, target)) yield r

  override def equals(other: Any) = {
    other match {
      case other: Ontology => {
        objects == other.objects && allGenerators == other.allGenerators && allRelations == other.allRelations
      }
      case _ => false
    }
  }
  
  trait OppositeFinitelyPresentedCategory extends FinitelyPresentedCategory with OppositeFinitelyGeneratedCategory { 
    override def relations(source: O, target: O) = for((Path(_, _, g1s), Path(_, _, g2s)) <- fpCategory.relations(target, source)) yield {
      (Path(source, target, g1s.reverse.map(reverseGenerator(_))), Path(source, target, g2s.reverse.map(reverseGenerator(_))))
    }
  }
}


