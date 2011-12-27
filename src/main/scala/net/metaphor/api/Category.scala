package net.metaphor.api

trait Category[C <: Category[C]] { self: C =>
  type O
  type M
  def identity(o: O): M
  def source(m: M): O
  def target(m: M): O
  def compose(m1: M, m2: M): M
  def compose(m0: M, ms: M*): M = ms.fold(m0)(compose _)
  def compose(o: O, ms: List[M]): M = {
    ms match {
      case Nil => identity(o)
      case m :: Nil => m
      case h :: t => compose(h, t:_*)
    }
  }

  
  trait FunctorFrom[TC <: Category[TC]] extends HeteroFunctor[C, TC] {
    override val source: self.type = self
  }
  trait NaturalTransformationFrom[TC <: Category[TC], F <: FunctorFrom[TC]] extends HeteroNaturalTransformation[C, TC, F] {
    override val source: F
    override val target: F
  }
  trait FunctorTo[SC <: Category[SC]] extends HeteroFunctor[SC, C] {
    override val target: self.type = self
  }
  trait NaturalTransformationTo[SC <: Category[SC], F <: FunctorTo[SC]] extends HeteroNaturalTransformation[SC, C, F] {
    override val source: F
    override val target: F
  }

  trait FunctorToSet extends FunctorFrom[Sets] with net.metaphor.api.FunctorToSet[C]

  trait NaturalTransformationToSet[F <: FunctorToSet] extends NaturalTransformationFrom[Sets, F] with net.metaphor.api.NaturalTransformationToSet[C, F] {
    override val source: F
    override val target: F
    override val sourceCategory: self.type = self
  }
}
