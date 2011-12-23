package net.metaphor.api

abstract class FunctorCategory[SO, SM, SC <: Category[SO, SM, SC], TO, TM, TC <: Category[TO, TM, TC], FF <: HeteroFunctor[SO, SM, SC, TO, TM, TC], TT <: HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC, FF], FC <: FunctorCategory[SO, SM, SC, TO, TM, TC, FF, TT, FC]](source: SC, target: TC) extends LargeCategory[FF, TT, FC] { self: FC =>
  override def identity(o: FF) = lift(new NaturalTransformation.IdentityHeteroNaturalTransformation(o))
  override def source(m: TT) = {
    m.source
  }
  override def target(m: TT) = {
    m.target
  }
  override def compose(m1: TT, m2: TT) = lift(new HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC, FF] {
    def source = m1.source
    def target = m2.target

    def apply(o: SO) = target.target.compose(m1(o), m2(o))
  })

  def lift(t: HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC, FF]): TT  
}
