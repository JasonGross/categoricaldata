package net.metaphor

package object api {
	def ??? = throw new NoSuchMethodException
	
	type FunctorToSet[O, M, C <: Category[O, M]] = HeteroFunctor[O, M, C, Set[Any], Function[Any, Any], Sets]
	type TransformationToSet[O, M, C <: Category[O, M]] = HeteroFunctor[O, M, C, Set[Any], Function[Any, Any], Sets]
}