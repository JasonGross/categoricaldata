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

  lazy val yoneda = new HeteroFunctor[O, M, C, F, T, CSets] {
    override def source = self
    override def target = functorsToSet
    override def onObjects(s: O) = lift(new FunctorToSet {
      override def onObjects(t: O) = normalWords(s, t)
      override def onMorphisms(m: M) = { n: M => compose(n, m) }.asInstanceOf[Any => Any]
    })
   override def onMorphisms(m: M) = lift(new NaturalTransformationToSet[F] {
      override def source = onObjects(self.source(m))
      override def target = onObjects(self.target(m))
      override def apply(o: O) = ???
    })
  }
  
  type CO <: CategoryOver[O, M, C, FunctorTo[O, M, C]]
  type FO <: FunctorOver[O, M, C, Functor[O, M, C], FunctorTo[O, M, C], CO]
  type CsO <: CategoriesOver[O, M, C, Functor[O, M, C], FunctorTo[O, M, C], CO, FO, CsO]

  def categoriesOver: CsO
  
  lazy val slice = new HeteroFunctor[O, M, C, CO, FO, CsO] {
    override def source = self
    override def target = categoriesOver
    override def onObjects(s: O) = ???
    override def onMorphisms(m: M) = ???
  }
//  lazy val coslice = new HeteroFunctor[O, M, C, _, _ ,_] {
//    override def source = opposite
//    override def target = functorsOver
//    override def onObjects(s: O) = ???
//    override def onMorphisms(m: M) = ???
//  }
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
