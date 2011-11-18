package net.metaphor.api

class FunctorCategory[SO, SM, TO, TM](source: Category[SO, SM], target: Category[TO, TM]) extends Category[HeteroFunctor[SO, SM, TO, TM], HeteroNaturalTransformation[SO, SM, TO, TM]] {
  def identity(o: HeteroFunctor[SO, SM, TO, TM]) = new NaturalTransformation.IdentityHeteroNaturalTransformation(o)
  def compose(m1: HeteroNaturalTransformation[SO, SM, TO, TM], m2: HeteroNaturalTransformation[SO, SM, TO, TM]) = new HeteroNaturalTransformation[SO, SM, TO, TM] {
    def source = m1.source
    def target = m2.target

    def apply(o: SO) = target.target.compose(m1(o), m2(o))
  }
}

class FunctorsToSet[O, M, C <: Category[O, M]](source: C) extends FunctorCategory[O, M, Set, Function](source, Sets)

class PullbackFunctor[O1, M1, O2, M2](F: Functor[O1, M1], targetCategory: Category[O2, M2]) extends Functor[HeteroFunctor[O1, M1, O2, M2], HeteroNaturalTransformation[O1, M1, O2, M2]] {
  val source = new FunctorCategory[O1, M1, O2, M2](F.source, targetCategory)
  val target = new FunctorCategory[O1, M1, O2, M2](F.target, targetCategory)
  
  def onObjects(G: HeteroFunctor[O1, M1, O2, M2]) = new Categories.CompositeHeteroFunctor(F, G)
  def onMorphisms(T: HeteroNaturalTransformation[O1, M1, O2, M2]) = ???
}
class PullbackNaturalTransformation[O1, M1, O2, M2](T: NaturalTransformation[O1, M1], targetCategory: Category[O2, M2]) extends NaturalTransformation[HeteroFunctor[O1, M1, O2, M2], HeteroNaturalTransformation[O1, M1, O2, M2]] {
  val source = new PullbackFunctor[O1, M1, O2, M2](T.source, targetCategory)
  val target = new PullbackFunctor[O1, M1, O2, M2](T.target, targetCategory)
  
  def apply(o: HeteroFunctor[O1, M1, O2, M2]) = ???
}

trait CategoricalTwoFunctor[O1, M1, C1 <: Category[O1, M1], O2, M2, C2 <: Category[O2, M2]] extends HeteroTwoFunctor[C1, Functor[O1, M1], NaturalTransformation[O1, M1], C2, Functor[O2, M2], NaturalTransformation[O2, M2]]

class PullbackTwoFunctor[O, M](val source: FinitelyPresentedCategories[O, M]) extends CategoricalTwoFunctor[O, M, FinitelyPresentedCategory[O, M], HeteroFunctor[O, M, Set, Function], HeteroNaturalTransformation[O, M, Set, Function], FunctorsToSet[O, M, FinitelyPresentedCategory[O, M]]] {
  def target = ???
  
  def onZeroMorphisms(m0: FinitelyPresentedCategory[O, M]) = new FunctorsToSet(m0)
  def onOneMorphisms(m1: Functor[O, M]): Functor[HeteroFunctor[O, M, Set, Function], HeteroNaturalTransformation[O, M, Set, Function]] = new PullbackFunctor(m1, Sets)
  def onTwoMorphisms(m2: NaturalTransformation[O, M]): NaturalTransformation[HeteroFunctor[O, M, Set, Function], HeteroNaturalTransformation[O, M, Set, Function]] = new PullbackNaturalTransformation(m2, Sets)
}