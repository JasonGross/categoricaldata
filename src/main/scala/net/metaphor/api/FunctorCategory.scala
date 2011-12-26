package net.metaphor.api

abstract class FunctorCategory[SC <: Category[SC], TC <: Category[TC], FC <: FunctorCategory[SC, TC, FC]](source: SC, target: TC) extends Category[FC] { self: FC =>
  override type O <: HeteroFunctor[SC, TC]
  override type M <: HeteroNaturalTransformation[SC, TC, O]
  override def identity(o: O) = lift(new NaturalTransformation.IdentityHeteroNaturalTransformation(o))
  override def source(m: M) = {
    m.source
  }
  override def target(m: M) = {
    m.target
  }
  override def compose(m1: M, m2: M) = lift(new HeteroNaturalTransformation[SC, TC, O] {
    val source = m1.source
    val target = m2.target

    def apply(o: sourceCategory.O) = target.target.compose(
        m1(o.asInstanceOf[m1.sourceCategory.O]).asInstanceOf[this.target.target.M],
        m2(o.asInstanceOf[m2.sourceCategory.O]).asInstanceOf[this.target.target.M]).asInstanceOf[targetCategory.M]
  })

  def lift(t: HeteroNaturalTransformation[SC, TC, O]): M  
}
