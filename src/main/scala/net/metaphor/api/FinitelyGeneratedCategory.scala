package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait FinitelyGeneratedCategory[O, M, C <: FinitelyGeneratedCategory[O, M, C]] extends Category[O, M, C] { self: C =>
  def objects: List[O]
  def generators(source: O, target: O): List[M]
  def generatorsFrom(source: O) = for (target <- objects; g <- generators(source, target)) yield g
  def generatorsTo(target: O) = for (source <- objects; g <- generators(source, target)) yield g
  def allGenerators: List[M] = for (source <- objects; target <- objects; g <- generators(source, target)) yield g

  def wordsOfLength(k: Int)(source: O, target: O): List[M] = {
    k match {
      case 0 => List(identity(source))
      case 1 => generators(source, target)
      case _ => for (other <- objects; g <- generators(other, target); w <- wordsOfLength(k - 1)(source, other)) yield compose(w, g)
    }
  }
  def allWordsOfLength(k: Int): List[M] = {
    for(s <- objects; t <- objects; w <- wordsOfLength(k)(s,t)) yield w
  }
  def words(source: O, target: O) = (for (k <- NonStrictNaturalNumbers) yield wordsOfLength(k)(source, target)).takeWhile(_.nonEmpty).flatten
  def allWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k)).takeWhile(_.nonEmpty).flatten
  def allNontrivialWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k + 1)).takeWhile(_.nonEmpty).flatten
}

trait FinitelyGeneratedCategories[O, M, C <: FinitelyGeneratedCategory[O, M, C]] /* extends Categories[O, M, C] */ { FGCAT =>
}
