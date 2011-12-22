package net.metaphor.api

trait HeteroNaturalTransformation[O1, M1, C1 <: Category[O1, M1, C1], O2, M2, C2 <: Category[O2, M2, C2], F <: HeteroFunctor[O1, M1, C1, O2, M2, C2]] {
  def source: F
  def target: F
  def sourceCategory = source.source
  def targetCategory = source.target
  def apply(o: O1): M2
}

trait NaturalTransformation[O, M, C <: Category[O, M, C], F <: Functor[O, M, C]] extends HeteroNaturalTransformation[O, M, C, O, M, C, F] {
  def source: F
  def target: F
}

object NaturalTransformation {
  class IdentityHeteroNaturalTransformation[O1, M1, C1 <: Category[O1, M1, C1], O2, M2, C2 <: Category[O2, M2, C2], F <: HeteroFunctor[O1, M1, C1, O2, M2, C2]](functor: F) extends HeteroNaturalTransformation[O1, M1, C1, O2, M2, C2, F] {
    def source = functor
    def target = functor
    def apply(o: O1) = functor.target.identity(functor(o))
  }
//  class IdentityNaturalTransformation[O, M, C <: Category[O, M, C], F <: Functor[O, M, C]](functor: F) extends NaturalTransformation[O, M, C, F] {
//    def source = functor
//    def target = functor
//    def apply(o: O) = functor.target.identity(functor(o))
//  }
}