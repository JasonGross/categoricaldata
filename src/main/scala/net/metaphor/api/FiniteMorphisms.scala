package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait NormalForm { fpCategory: FinitelyPresentedCategory =>
  def normalForm(p: Path): Path

  def normalWordsOfLength(k: Int)(source: fpCategory.O, target: fpCategory.O): List[Path] = {
    wordsOfLength(k)(source, target).filter(inNormalForm _)
  }

  def inNormalForm(p: Path) = p == normalForm(p)
  override def pathEquality(p1: fpCategory.Path, p2: fpCategory.Path) = {
    normalForm(p1) == normalForm(p2)
  }
}

trait FiniteMorphisms extends NormalForm { fpCategory: FinitelyPresentedCategory =>
  // TODO How would instances arise, which aren't acyclic?
  // We'd need a proof there are only finitely many loops/relations

  def maximumWordLength(source: fpCategory.O, target: fpCategory.O): Int
  def maximumWordLength: Int = (for (s <- objects; t <- objects) yield maximumWordLength(s, t)).max
  def normalWords(source: fpCategory.O, target: fpCategory.O) = (for (k <- 0 to maximumWordLength(source, target); w <- normalWordsOfLength(k)(source, target)) yield w).toList

  override def normalForm(p: Path): Path = ??? // FIXME, how do we obtain normal forms? //???


trait Acyclic extends FiniteMorphisms { fpCategory: FinitelyPresentedCategory =>
  def verifyAcyclicity: Boolean = {
    for (w <- allNontrivialWords) {
      if (w.source == w.target) return false
    }
    return true
  }

  require(verifyAcyclicity)

  override def maximumWordLength(source: O, target: O): Int = ??? //???
  override def normalForm(p: Path): Path = p // FIXME: for acyclic categories, each hom space is finite, and we can brute force normal forms
}

trait Free { fpCategory: FinitelyPresentedCategory =>
  require(allRelations.isEmpty)
}

trait FreeAcyclic extends Free with Acyclic { fpCategory: FinitelyPresentedCategory =>
  override def normalForm(p: Path) = p
}
