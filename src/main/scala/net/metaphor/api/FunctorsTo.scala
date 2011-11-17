package net.metaphor.api

trait FunctorCategory[SO, SM, TO, TM] extends Category[HeteroFunctor[SO, SM, TO, TM], HeteroNaturalTransformation[SO, SM, TO, TM] with Morphism]

class FunctorsTo[TO, TM <: Morphism](target: Category[TO, TM]) {
  def apply[O, M <: Morphism](source: Category[O, M]): FunctorCategory[O, M, TO, TM] = new FunctorCategory[O, M, TO, TM] {
    def identity(o: HeteroFunctor[O, M, TO, TM]) = ???
    def compose(m1: HeteroNaturalTransformation[O, M, TO, TM] with Morphism, m2: HeteroNaturalTransformation[O, M, TO, TM] with Morphism) = ???
  }
}

object FunctorsToSets extends FunctorsTo(Sets)

object FunctorsTo {
 def apply[O, M <: Morphism, C <: Category[O, M], TO, TM <: Morphism](target: Category[TO, TM])(_source: Categories[O, M, C]): HeteroTwoFunctor[X] = new HeteroTwoFunctor[X] {
   def source = _source
   def target = ???
   
   def apply(c: C) = FunctorsTo(target)(source)
   def apply(f: Functor[O, M]) = ???
   def apply(t: NaturalTransformation[O, M]) = ???
 }
}