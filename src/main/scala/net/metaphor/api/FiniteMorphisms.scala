package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait NormalForm { self: FinitelyPresentedCategory =>
  def normalForm(p: Path): Path

  def normalWordsOfLength(k: Int)(source: self.O, target: self.O): List[Path] = {
    wordsOfLength(k)(source, target).filter(inNormalForm _)
  }

  def inNormalForm(p: Path) = p == normalForm(p)
  override def pathEquality(p1: self.Path, p2: self.Path) = {
    normalForm(p1) == normalForm(p2)
  }
}

trait FiniteMorphisms extends NormalForm { self: FinitelyPresentedCategory =>
  // TODO How would instances arise, which aren't acyclic?
  // We'd need a proof there are only finitely many loops/relations

  def maximumWordLength(source: self.O, target: self.O): Int
  def maximumWordLength: Int = (for (s <- objects; t <- objects) yield maximumWordLength(s, t)).max
  def normalWords(source: self.O, target: self.O) = (for (k <- 0 to maximumWordLength(source, target); w <- normalWordsOfLength(k)(source, target)) yield w).toList

  override def normalForm(p: Path): Path = ??? // FIXME, how do we obtain normal forms?
}

trait Acyclic extends FiniteMorphisms { self: FinitelyPresentedCategory =>
  def verifyAcyclicity: Boolean = {
    for (w <- allNontrivialWords) {
      if (w.source == w.target) return false
    }
    return true
  }

  require(verifyAcyclicity)

  override def maximumWordLength(source: O, target: O): Int = ???
  override def normalForm(p: Path): Path = p // FIXME: for acyclic categories, each hom space is finite, and we can brute force normal forms
}

trait Free { self: FinitelyPresentedCategory =>
  require(allRelations.isEmpty)
}

trait FreeAcyclic extends Free with Acyclic { self: FinitelyPresentedCategory =>
  override def normalForm(p: Path) = p
}
