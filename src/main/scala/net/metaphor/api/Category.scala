package net.metaphor.api

trait Category[O, M, C <: Category[O, M, C]] { self: C =>
  def identity(o: O): M
  def source(m: M): O
  def target(m: M): O
  def compose(m1: M, m2: M): M

  trait FunctorFrom[TO, TM, TC <: Category[TO, TM, TC]] extends HeteroFunctor[O, M, C, TO, TM, TC] {
    override val source = self
  }
  trait NaturalTransformationFrom[TO, TM, TC <: Category[TO, TM, TC], F <: FunctorFrom[TO, TM, TC]] extends HeteroNaturalTransformation[O, M, C, TO, TM, TC, F] {
    override def source: F
    override def target: F
  }
  trait FunctorTo[SO, SM, SC <: Category[SO, SM, SC]] extends HeteroFunctor[SO, SM, SC, O, M, C] {
    override val target = self
  }
  trait NaturalTransformationTo[SO, SM, SC <: Category[SO, SM, SC], F <: FunctorTo[SO, SM, SC]] extends HeteroNaturalTransformation[SO, SM, SC, O, M, C, F] {
    override def source: F
    override def target: F
  }

  trait FunctorToSet extends FunctorFrom[Set, Function, Sets] with net.metaphor.api.FunctorToSet[O, M, C] 

  trait NaturalTransformationToSet[F <: FunctorToSet] extends NaturalTransformationFrom[Set, Function, Sets, F] with net.metaphor.api.NaturalTransformationToSet[O, M, C, F] {
    override def source: F
    override def target: F
  }

  abstract class FunctorsToSet[F <: FunctorToSet, T <: NaturalTransformationToSet[F], FC <: FunctorsToSet[F, T, FC]] extends net.metaphor.api.FunctorsToSet[O, M, C, F, T, FC](self) { functorsToSet: FC => }
}