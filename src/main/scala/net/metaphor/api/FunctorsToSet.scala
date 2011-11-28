package net.metaphor.api

class FunctorCategory[SO, SM, SC <: Category[SO, SM], TO, TM, TC <: Category[TO, TM]](source: SC, target: TC) extends Category[HeteroFunctor[SO, SM, SC, TO, TM, TC], HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC]] {
  def identity(o: HeteroFunctor[SO, SM, SC, TO, TM, TC]) = new NaturalTransformation.IdentityHeteroNaturalTransformation(o)
  def source(m: HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC]) = {
    m.source
  }
  def target(m: HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC]) = {
    m.target
  }
  def compose(m1: HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC], m2: HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC]) = new HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC] {
    def source = m1.source
    def target = m2.target

    // FIXME code smell; this is duplicated in Composite2NaturalTransformation.apply
    def apply(o: SO) = target.target.compose(m1(o), m2(o))
  }
}

class FunctorsToSet[O, M, C <: Category[O, M]](source: C) extends FunctorCategory[O, M, C, Set, Function, Sets](source, Sets)

class PullbackFunctor[O1, M1, C1 <: Category[O1, M1], O2, M2, C2 <: Category[O2, M2]](F: Functor[O1, M1, C1], targetCategory: C2) extends Functor[HeteroFunctor[O1, M1, C1, O2, M2, C2], HeteroNaturalTransformation[O1, M1, C1, O2, M2, C2], FunctorCategory[O1, M1, C1, O2, M2, C2]] {
  val source = new FunctorCategory[O1, M1, C1, O2, M2, C2](F.source, targetCategory)
  val target = new FunctorCategory[O1, M1, C1, O2, M2, C2](F.target, targetCategory)

  def onObjects(G: HeteroFunctor[O1, M1, C1, O2, M2, C2]) = new Categories.CompositeHeteroFunctor(F, G)
  def onMorphisms(T: HeteroNaturalTransformation[O1, M1, C1, O2, M2, C2]) = ???
}
class PullbackNaturalTransformation[O1, M1, C1 <: Category[O1, M1], O2, M2, C2 <: Category[O2, M2]](T: NaturalTransformation[O1, M1, C1], targetCategory: C2) extends NaturalTransformation[HeteroFunctor[O1, M1, C1, O2, M2, C2], HeteroNaturalTransformation[O1, M1, C1, O2, M2, C2], FunctorCategory[O1, M1, C1, O2, M2, C2]] {
  val source = new PullbackFunctor[O1, M1, C1, O2, M2, C2](T.source, targetCategory)
  val target = new PullbackFunctor[O1, M1, C1, O2, M2, C2](T.target, targetCategory)

  def apply(o: HeteroFunctor[O1, M1, C1, O2, M2, C2]) = ???
}

trait CategoricalTwoFunctor[O1, M1, C1 <: Category[O1, M1], O2, M2, C2 <: Category[O2, M2]] extends HeteroTwoFunctor[C1, Functor[O1, M1, C1], NaturalTransformation[O1, M1, C1], Categories[O1, M1, C1], C2, Functor[O2, M2, C2], NaturalTransformation[O2, M2, C2], Categories[O2, M2, C2]]

class PullbackTwoFunctor[O, M, C <: FinitelyPresentedCategory[O, M]](val source: FinitelyPresentedCategories[O, M, C]) extends CategoricalTwoFunctor[O, M, C, HeteroFunctor[O, M, C, Set, Function, Sets], HeteroNaturalTransformation[O, M, C, Set, Function, Sets], FunctorCategory[O, M, C, Set, Function, Sets]] {
  def target = ???

  def onZeroMorphisms(m0: C) = new FunctorsToSet[O, M, C](m0)
  def onOneMorphisms(m1: Functor[O, M, C]): Functor[HeteroFunctor[O, M, C, Set, Function, Sets], HeteroNaturalTransformation[O, M, C, Set, Function, Sets], FunctorCategory[O, M, C, Set, Function, Sets]] = new PullbackFunctor[O, M, C, Set, Function, Sets](m1, Sets)
  def onTwoMorphisms(m2: NaturalTransformation[O, M, C]): NaturalTransformation[HeteroFunctor[O, M, C, Set, Function, Sets], HeteroNaturalTransformation[O, M, C, Set, Function, Sets], FunctorCategory[O, M, C, Set, Function, Sets]] = new PullbackNaturalTransformation[O, M, C, Set, Function, Sets](m2, Sets)
}