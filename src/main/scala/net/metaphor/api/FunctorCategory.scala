package net.metaphor.api

abstract class FunctorCategory[SC <: Category[SC], TC <: Category[TC], FF <: HeteroFunctor[SC, TC], TT <: HeteroNaturalTransformation[SC, TC, FF], FC <: FunctorCategory[SC, TC, FF, TT, FC]](source: SC, target: TC) extends LargeCategory[FC] { self: FC =>
  type O = FF
  type M = TT
  override def identity(o: FF) = lift(new NaturalTransformation.IdentityHeteroNaturalTransformation(o))
  override def source(m: TT) = {
    m.source
  }
  override def target(m: TT) = {
    m.target
  }
  override def compose(m1: TT, m2: TT) = lift(new HeteroNaturalTransformation[SC, TC, FF] {
    val source = m1.source
    val target = m2.target

    def apply(o: sourceCategory.O) = target.target.compose(
        m1(o.asInstanceOf[m1.sourceCategory.O]).asInstanceOf[this.target.target.M],
        m2(o.asInstanceOf[m2.sourceCategory.O]).asInstanceOf[this.target.target.M]).asInstanceOf[targetCategory.M]
  })

  def lift(t: HeteroNaturalTransformation[SC, TC, FF]): TT  
}
