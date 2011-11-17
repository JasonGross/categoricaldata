package net.metaphor.api

trait Set
trait Function extends Morphism

object Sets extends Category[Set, Function] {
	def identity(set: Set) = ???
	def compose(first: Function, second: Function) = ???
}

//trait FunctorToSet[O, M <: Morphism] extends HeteroFunctor[O, M, Set, Function]
//trait NaturalTransformationToSet[O, M <: Morphism] extends HeteroNaturalTransformation[O, M, Set, Function]