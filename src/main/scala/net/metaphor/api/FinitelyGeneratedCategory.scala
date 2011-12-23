package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait LocallyFinitelyGeneratedCategory[O, M, C <: LocallyFinitelyGeneratedCategory[O, M, C]] extends Category[O, M, C] { self: C =>
  def objectsAtLevel(k: Int): List[O]

  def generators(source: O, target: O): List[M]
  def generatorsFrom(source: O): List[M]
  def generatorsTo(source: O): List[M]

  def wordsOfLength(k: Int)(source: O, target: O): List[M] = {
    k match {
      case 0 => List(identity(source))
      case 1 => generators(source, target)
      case _ => for (g <- generatorsTo(target); w <- wordsOfLength(k - 1)(source, self.source(g))) yield compose(w, g)
    }
  }

  trait Opposite { opposite: C =>
    // reverse all the levels!
    override def objectsAtLevel(k: Int) = self.objectsAtLevel(-k)
    override def generators(source: O, target: O) = self.generators(target, source)

    override def source(m: M) = self.target(m)
    override def target(m: M) = self.source(m)
    override def compose(m1: M, m2: M) = self.compose(m2, m1)
  }

  /**
   * Implementing opposite should be easy; generally just write "def opposite = new C with Opposite", replacing C as appropriate.
   */
  def opposite: C

}

trait FinitelyGeneratedCategory[O, M, C <: FinitelyGeneratedCategory[O, M, C]] extends LocallyFinitelyGeneratedCategory[O, M, C] { self: C =>
  // TODO, maybe minimumLevel actually belongs one level up; we could insist everything is bounded below.
  val minimumLevel: Int
  val maximumLevel: Int

  def objects: List[O] = for (k <- (minimumLevel to maximumLevel).toList; o <- objectsAtLevel(k)) yield o

  def generatorsFrom(source: O) = for (target <- objects; g <- generators(source, target)) yield g
  def generatorsTo(target: O) = for (source <- objects; g <- generators(source, target)) yield g
  def allGenerators: List[M] = for (source <- objects; target <- objects; g <- generators(source, target)) yield g

  def allWordsOfLength(k: Int): List[M] = {
    for (s <- objects; t <- objects; w <- wordsOfLength(k)(s, t)) yield w
  }
  def words(source: O, target: O) = (for (k <- NonStrictNaturalNumbers) yield wordsOfLength(k)(source, target)).takeWhile(_.nonEmpty).flatten
  def allWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k)).takeWhile(_.nonEmpty).flatten
  def allNontrivialWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k + 1)).takeWhile(_.nonEmpty).flatten

  trait Opposite extends super.Opposite { opposite: C =>
    override val minimumLevel = self.minimumLevel
    override val maximumLevel = self.maximumLevel
  }
}

trait FinitelyGeneratedCategories[O, M, C <: FinitelyGeneratedCategory[O, M, C]] /* extends Categories[O, M, C] */ { FGCAT =>
}
