package net.metaphor.api

abstract class FunctorCategory[SO, SM, SC <: Category[SO, SM, SC], TO, TM, TC <: Category[TO, TM, TC], F <: HeteroFunctor[SO, SM, SC, TO, TM, TC], T<: HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC, F], FC <: FunctorCategory[SO, SM, SC, TO,TM, TC, F, T, FC]](source: SC, target: TC) extends Category[F, T, FC] { self: FC =>
  override def identity(o: F) = lift(new NaturalTransformation.IdentityHeteroNaturalTransformation(o))
  override def source(m: T) = {
    m.source
  }
  override def target(m: T) = {
    m.target
  }
  override def compose(m1: T, m2: T) = lift(new HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC, F] {
    def source = m1.source
    def target = m2.target

    def apply(o: SO) = target.target.compose(m1(o), m2(o))
  })
  
  def lift(t: HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC,F]): T
}
