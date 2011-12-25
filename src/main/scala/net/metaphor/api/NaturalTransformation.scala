package net.metaphor.api

trait HeteroNaturalTransformation[C1 <: Category[C1], C2 <: Category[C2], F <: HeteroFunctor[C1, C2]] {
  val source: F
  val target: F
  val sourceCategory: source.source.type = source.source
  val targetCategory: source.target.type = source.target
  def apply(o: sourceCategory.O): targetCategory.M
}

trait NaturalTransformation[C <: Category[C], F <: Functor[C]] extends HeteroNaturalTransformation[C,C, F] {
  val source: F
  val target: F
}

object NaturalTransformation {
  class IdentityHeteroNaturalTransformation[C1 <: Category[C1], C2 <: Category[C2], F <: HeteroFunctor[C1, C2]](val functor: F) extends HeteroNaturalTransformation[C1, C2, F] {
    val source: functor.type = functor
    val target: functor.type = functor
    override def apply(o: sourceCategory.O) = functor.target.identity(functor(o.asInstanceOf[functor.source.O])).asInstanceOf[targetCategory.M]
  }
//  class IdentityNaturalTransformation[O, M, C <: Category[O, M, C], F <: Functor[O, M, C]](functor: F) extends NaturalTransformation[O, M, C, F] {
//    def source = functor
//    def target = functor
//    def apply(o: O) = functor.target.identity(functor(o))
//  }
}