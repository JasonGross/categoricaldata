package net.metaphor.api

trait SmallCategory[C <: SmallCategory[C]] extends Category[C] { self: C =>
  type F <: FunctorToSet
  type T <: NaturalTransformationToSet[F]
  type CSets <: FunctorsToSet

  def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[C]): F
  def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[C, F]): T

  def functorsToSet: CSets

  class FunctorsToSet extends net.metaphor.api.FunctorsToSet[C, CSets](self) { functorsToSet: CSets =>
    override type O = F
    override type M = T

    def lift(t: HeteroNaturalTransformation[C, Sets, F]) = self.liftNaturalTransformationToSet(
      new net.metaphor.api.NaturalTransformationToSet[C, F] {
        override val source = t.source
        override val target = t.target
        override def apply(o: self.O) = t(o)
      })
  }

  trait FunctorTo[SC <: SmallCategory[SC]] extends SmallHeteroFunctor[SC, C] {
    override val target: self.type = self
  }
  trait NaturalTransformationTo[SC <: SmallCategory[SC], F <: FunctorTo[SC]] extends HeteroNaturalTransformation[SC, C, F] {
    override val source: F
    override val target: F
  }

  trait CategoryOver[SC <: SmallCategory[SC]] {
    def category = functor.source
    def functor: FunctorTo[SC]
  }
  trait FunctorOver[SC <: SmallCategory[SC]] {
    def source: CategoryOver[SC]
    def target: CategoryOver[SC]
    def functor: Functor[SC]
  }

  trait CategoriesOver[SC <: SmallCategory[SC]] extends Category[CategoriesOver[SC]] { categoriesOver =>
    override type O = CategoryOver[SC]
    override type M = FunctorOver[SC]
    override def identity(f: O) = new FunctorOver[SC] {
      val source = f
      val target = f
      var functor = new Functor.IdentityFunctor(f.category)
    }
    override def source(t: M) = t.source
    override def target(t: M) = t.target
    override def compose(m1: M, m2: M): M = new FunctorOver[SC] {
      val source = m1.source
      val target = m2.target
      val functor = new Functor.CompositeFunctor(m1.functor, m2.functor)
    }
  }

  def categoriesOver[SC <: SmallCategory[SC]]: CategoriesOver[SC] = new CategoriesOver[SC] {}
}