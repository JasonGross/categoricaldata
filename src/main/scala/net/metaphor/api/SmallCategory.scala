package net.metaphor.api

trait SmallCategory extends Category { category =>

  trait FunctorToSet extends FunctorFrom with net.metaphor.api.FunctorToSet

  type F <: FunctorToSet
  type T <: NaturalTransformationToSet

  trait NaturalTransformationToSet extends NaturalTransformationFrom with net.metaphor.api.NaturalTransformationToSet {
    override val source: F
    override val target: F
    override val sourceCategory: category.type = category
  }

  def internalize(f: net.metaphor.api.FunctorToSet): F
  def internalize(t: net.metaphor.api.NaturalTransformationToSet): T

  class SpecializedFunctorsToSet extends Category { functorsToSet =>
    override type O = category.F
    override type M = category.T

    override def identity(o: O) = internalize(FunctorsToSet.identity(o))
    override def source(m: M) = internalize(FunctorsToSet.source(m))
    override def target(m: M) = internalize(FunctorsToSet.target(m))
    override def compose(m1: M, m2: M) = internalize(FunctorsToSet.compose(m1, m2))
  }

  object AllFunctorsToSet extends Category {
    override type O = category.FunctorToSet
    override type M = category.NaturalTransformationToSet

    override def identity(o: O) = internalize(FunctorsToSet.identity(o))
    override def source(m: M) = internalize(FunctorsToSet.source(m))
    override def target(m: M) = internalize(FunctorsToSet.target(m))
    override def compose(m1: M, m2: M) = internalize(FunctorsToSet.compose(m1, m2))
  }

  def functorsToSet: SpecializedFunctorsToSet

  trait FunctorTo extends SmallFunctor {
    override val target: category.type = category
  }
  trait NaturalTransformationTo extends NaturalTransformation {
    override val source: FunctorTo
    override val target: FunctorTo
  }

  trait CategoryOver {
    def category = functor.source
    def functor: FunctorTo
  }
  trait FunctorOver {
    def source: CategoryOver
    def target: CategoryOver
    def functor: Functor
  }

  trait CategoriesOver extends Category { categoriesOver =>
    override type O = CategoryOver
    override type M = FunctorOver
    override def identity(f: O) = new FunctorOver {
      val source = f
      val target = f
      var functor = new Functor.IdentityFunctor(f.category)
    }
    override def source(t: M) = t.source
    override def target(t: M) = t.target
    override def compose(m1: M, m2: M): M = new FunctorOver {
      val source = m1.source
      val target = m2.target
      val functor = new Functor.CompositeFunctor(m1.functor, m2.functor)
    }
  }

  def categoriesOver: CategoriesOver = new CategoriesOver {}
}
