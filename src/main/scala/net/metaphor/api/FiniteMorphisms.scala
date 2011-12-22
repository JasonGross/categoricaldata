package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait SolvableWordProblem[O, M, C <: FinitelyPresentedCategory[O, M, C]] { self: C =>
  def normalForm(m: M): M

  def normalWordsOfLength(k: Int)(source: O, target: O): List[M] = {
    wordsOfLength(k)(source, target).filter(inNormalForm _)
  }

  def inNormalForm(m: M) = m == normalForm(m)
}

trait FiniteMorphisms[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends SolvableWordProblem[O, M, C] { self: C =>
  // TODO How could we ever come across instances of these, but which aren't acyclic?
  // We'd need a proof there are only finitely many loops/relations

  def maximalWordLength(source: O, target: O): Int
  def normalWords(source: O, target: O) = (for (k <- 0 to maximalWordLength(source, target); w <- normalWordsOfLength(k)(source, target)) yield w).toList

  def yoneda(s: O) = new FunctorToSet {
    override def onObjects(t: O) = normalWords(s, t)
    override def onMorphisms(m: M) = { n: M => compose(n, m) }.asInstanceOf[Any => Any]
  }
}

trait Acyclic[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FiniteMorphisms[O, M, C] { self: C =>
  def verifyAcyclicity: Boolean = {
    for (w <- allNontrivialWords) {
      if (source(w) == target(w)) return false
    }
    return true
  }

  require(verifyAcyclicity)

  override def maximalWordLength(source: O, target: O): Int = ???
  override def normalForm(m: M): M = ???
}

trait Graph[O, M, C <: FinitelyPresentedCategory[O, M, C]] { self: C =>
  require(allRelations.isEmpty)
}

trait AcyclicGraph[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends Graph[O, M, C] with Acyclic[O, M, C] { self: C =>
  override def normalForm(m: M) = m
}
