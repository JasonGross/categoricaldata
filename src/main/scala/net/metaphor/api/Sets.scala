package net.metaphor.api

trait Set
trait Function

object Sets extends Category[Set, Function] {
	def identity(set: Set) = ???
	def compose(first: Function, second: Function) = ???
}

trait FunctorToSet[O, M] extends HeteroFunctor[O, M, Set, Function]

object Colimit {
  // TODO add more type parameters to Functor, so we can ensure that the source category is FinitelyPresented.
  def apply[O, M](diagram: FunctorToSet[O, M]) = ???
}
