package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait NormalForm[C <: FinitelyPresentedCategory[C]] { self: C =>
  def normalForm(p: Path): Path

  def normalWordsOfLength(k: Int)(source: self.O, target: self.O): List[Path] = {
    wordsOfLength(k)(source, target).filter(inNormalForm _)
  }

  def inNormalForm(p: Path) = p == normalForm(p)
  override def pathEquality(p1: self.Path, p2: self.Path) = {
    normalForm(p1) == normalForm(p2)
  }
}

trait FiniteMorphisms[C <: FinitelyPresentedCategory[C]] extends NormalForm[C] { self: C =>
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
  
    override def normalForm(p: Path): Path = ???

}

trait Acyclic[C <: FinitelyPresentedCategory[C]] extends FiniteMorphisms[C] { self: C =>
  def verifyAcyclicity: Boolean = {
    for (w <- allNontrivialWords) {
      if (w.source == w.target) return false
    }
    return true
  }

  require(verifyAcyclicity)

  override def maximumWordLength(source: O, target: O): Int = ???
  override def normalForm(p: Path): Path = ???
}

trait Graph[C <: FinitelyPresentedCategory[C]] { self: C =>
  require(allRelations.isEmpty)
}

trait AcyclicGraph[C <: FinitelyPresentedCategory[C]] extends Graph[C] with Acyclic[C] { self: C =>
  override def normalForm(p: Path) = p
}
