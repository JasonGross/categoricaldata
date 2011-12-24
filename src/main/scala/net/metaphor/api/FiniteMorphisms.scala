package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait SolvableWordProblem[C <: FinitelyPresentedCategory[C]] { self: C =>
  def normalForm(m: self.M): self.M

  def normalWordsOfLength(k: Int)(source: self.O, target: self.O): List[self.M] = {
    wordsOfLength(k)(source, target).filter(inNormalForm _)
  }

  def inNormalForm(m: self.M) = m == normalForm(m)
}

trait FiniteMorphisms[C <: FinitelyPresentedCategory[C]] extends SolvableWordProblem[C] { self: C =>
  // TODO How would instances arise, which aren't acyclic?
  // We'd need a proof there are only finitely many loops/relations

  def maximumWordLength(source: self.O, target: self.O): Int
  def maximumWordLength: Int = (for (s <- objects; t <- objects) yield maximumWordLength(s, t)).max
  def normalWords(source: self.O, target: self.O) = (for (k <- 0 to maximumWordLength(source, target); w <- normalWordsOfLength(k)(source, target)) yield w).toList

  // FIXME uncomment this!
//    lazy val yoneda = new HeteroFunctor[C, CSets] {
//      override def source = self
//      override def target = functorsToSet
//      override def onObjects(s: self.O) = liftFunctorToSet(new FunctorToSet {
//        override def onObjects(t: O) = normalWords(s, t)
//        override def onMorphisms(m: M) = { n: M => compose(n, m) }.asInstanceOf[Any => Any]
//      })
//     override def onMorphisms(m: self.M) = liftNaturalTransformationToSet(new NaturalTransformationToSet[F] {
//        override def source = onObjects(self.source(m))
//        override def target = onObjects(self.target(m))
//        override def apply(o: O) = ???
//      })
//    }
}

trait Acyclic[C <: FinitelyPresentedCategory[C]] extends FiniteMorphisms[C] { self: C =>
  def verifyAcyclicity: Boolean = {
    for (w <- allNontrivialWords) {
      if (source(w) == target(w)) return false
    }
    return true
  }

  require(verifyAcyclicity)

  override def maximumWordLength(source: O, target: O): Int = ???
  override def normalForm(m: M): M = ???
}

trait Graph[C <: FinitelyPresentedCategory[C]] { self: C =>
  require(allRelations.isEmpty)
}

trait AcyclicGraph[C <: FinitelyPresentedCategory[C]] extends Graph[C] with Acyclic[C] { self: C =>
  override def normalForm(m: M) = m
}
