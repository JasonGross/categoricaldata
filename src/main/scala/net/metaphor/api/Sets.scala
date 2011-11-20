package net.metaphor.api

trait Set
trait Function

trait Sets extends Category[Set, Function]

object Sets extends Sets {
  def identity(set: Set) = ???
  def compose(first: Function, second: Function) = ???
}

trait FunctorToSet[O, M, C <: Category[O, M]] extends HeteroFunctor[O, M, C, Set, Function, Sets]

object Colimit {
  // TODO add more type parameters to Functor, so we can ensure that the source category is FinitelyPresented.
  /**
   *
   *
   * returns a pair (s: Set, fs: List[Function]), where s is the colimit, and fs are the functions from the objects of functor.source to the colimit
   */
  def apply[O, M, C <: FinitelyPresentedCategory[O, M]](functor: FunctorToSet[O, M, C]): (Set, List[Function]) = {
    val diagram = functor.source
    ???
  }
}
