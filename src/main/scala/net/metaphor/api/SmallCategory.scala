package net.metaphor.api

trait SmallCategory extends Category { smallCategory =>

  trait FunctorToSet extends FunctorFrom with net.metaphor.api.FunctorToSet

  type F <: FunctorToSet
  type T <: NaturalTransformationToSet

  trait NaturalTransformationToSet extends NaturalTransformationFrom with net.metaphor.api.NaturalTransformationToSet {
    override val source: F
    override val target: F
  }

  def internalize(f: net.metaphor.api.FunctorToSet): F
  def internalize(t: net.metaphor.api.NaturalTransformationToSet): T

  class SpecializedFunctorsToSet extends Category { functorsToSet =>
    override type O = smallCategory.F
    override type M = smallCategory.T

    override def identity(o: O) = internalize(FunctorsToSet.identity(o))
    override def source(m: M) = internalize(FunctorsToSet.source(m))
    override def target(m: M) = internalize(FunctorsToSet.target(m))
    override def compose(m1: M, m2: M) = internalize(FunctorsToSet.compose(m1, m2))
  }

  object AllFunctorsToSet extends Category {
    override type O = smallCategory.FunctorToSet
    override type M = smallCategory.NaturalTransformationToSet

    override def identity(o: O) = internalize(FunctorsToSet.identity(o))
    override def source(m: M) = internalize(FunctorsToSet.source(m))
    override def target(m: M) = internalize(FunctorsToSet.target(m))
    override def compose(m1: M, m2: M) = internalize(FunctorsToSet.compose(m1, m2))
  }

  def functorsToSet: SpecializedFunctorsToSet

  trait CategoryOver extends SmallFunctor { categoryOver =>
    override val target: smallCategory.type = smallCategory
    trait Identity extends FunctorOver {
      override val source: categoryOver.type = categoryOver
      override val target: categoryOver.type = categoryOver
      override val functor = new F {
        override def onObjects(o: categoryOver.source.O) = o
        override def onMorphisms(m: categoryOver.source.M) = m
      }
    }
  }
  trait NaturalTransformationOver extends NaturalTransformation {
    override val source: CategoryOver
    override val target: CategoryOver
  }

  trait FunctorOver { functorOver =>
    val source: CategoryOver
    val target: CategoryOver
    trait F extends Functor {
      override val source: functorOver.source.source.type = functorOver.source.source
      override val target: functorOver.target.source.type = functorOver.target.source
    }
    def functor: F
  }

  trait CategoriesOver extends Category { categoriesOver =>
    override type O = CategoryOver
    override type M = FunctorOver
    override def identity(f: O) = new f.Identity {}
    override def source(t: M) = t.source
    override def target(t: M) = t.target
    override def compose(m1: M, m2: M): M = new FunctorOver {
      val source = m1.source
      val target = m2.target
      val functor = ??? // it's unclear to me that this is even possible to implement.
    }
  }

  def categoriesOver: CategoriesOver = new CategoriesOver {}
}

object SmallCategories {
  trait StandardFunctorsToSet { C: SmallCategory =>
    val functorsToSet = new SpecializedFunctorsToSet

    override type F = FunctorToSet
    override type T = NaturalTransformationToSet

    def internalize(f: net.metaphor.api.FunctorToSet): F = new FunctorToSet {
      require(f.source == source)
      def onObjects(o: O) = f(o.asInstanceOf[f.source.O])
      def onMorphisms(m: M) = f(m.asInstanceOf[f.source.M])
    }
    def internalize(t: net.metaphor.api.NaturalTransformationToSet): T = new NaturalTransformationToSet {
      require(t.sourceCategory == source)
      val source = internalize(t.source)
      val target = internalize(t.target)
      def apply(o: O) = t(o.asInstanceOf[t.sourceCategory.O])
    }
  }
}