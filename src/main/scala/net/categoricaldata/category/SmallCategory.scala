package net.categoricaldata.category

import net.categoricaldata.sets._
import net.categoricaldata.universalalgebra._

trait SmallCategory extends Category { smallCategory =>
  
  trait FunctorToSet extends FunctorFrom with net.categoricaldata.category.FunctorToSet with Functor.withSmallSource {
//    Commenting out the following line, things still compile, but we get AbstractMethodError everywhere:
    override val source: smallCategory.type = smallCategory
  }

  type F <: FunctorToSet
  type T <: NaturalTransformationToSet

  trait NaturalTransformationToSet extends NaturalTransformationFrom with net.categoricaldata.category.NaturalTransformationToSet {
    override val source: F
    override val target: F
  }

  def internalize(f: net.categoricaldata.category.FunctorToSet): F
  def internalize(t: net.categoricaldata.category.NaturalTransformationToSet): T

  trait FunctorsToSet extends net.categoricaldata.category.FunctorsToSet with InitialObject with TerminalObject with Products with Coproducts {
    override type O >: F <: smallCategory.FunctorToSet
    override type M >: T <: smallCategory.NaturalTransformationToSet

    override def internalize(f: net.categoricaldata.category.FunctorToSet) = smallCategory.internalize(f)
    override def internalize(t: net.categoricaldata.category.NaturalTransformationToSet) = smallCategory.internalize(t)

    override def terminalObject = internalize(new FunctorToSet {
      override def onObjects(o: smallCategory.O) = Sets.terminalObject
      override def onMorphisms(m: smallCategory.M) = Sets.morphismToTerminalObject(Sets.terminalObject)
    })
    override def morphismToTerminalObject(f: O) = internalize(new net.categoricaldata.category.NaturalTransformationToSet { t =>
      override val source = f
      override val target = terminalObject
      override def apply(o: t.sourceCategory.O) = Sets.morphismToTerminalObject(f(o))
    })
    override def initialObject = internalize(new FunctorToSet {
      override def onObjects(o: smallCategory.O) = Sets.initialObject
      override def onMorphisms(m: smallCategory.M) = Sets.morphismFromInitialObject(Sets.initialObject)
    })
    override def morphismFromInitialObject(f: O) = internalize(new net.categoricaldata.category.NaturalTransformationToSet { t =>
      override val source = initialObject
      override val target = f
      override def apply(o: t.sourceCategory.O) = Sets.morphismFromInitialObject(f(o))
    })

    override def product(xs: O*) = internalize(new FunctorToSet {
      override def onObjects(o: smallCategory.O) = Sets.product(xs.map(_(o)):_*)
      override def onMorphisms(m: smallCategory.M) = Sets.product(xs.map(_(m)):_*)
    })
    override def productProjections(xs: O*) = xs.toList.zipWithIndex map {
      case (x, i) => internalize(new net.categoricaldata.category.NaturalTransformationToSet { t =>
        override val source = product(xs:_*)
        override val target = x
        override def apply(o: t.sourceCategory.O) = Sets.productProjections(xs.map(_(o)))(i)
      })
    }
    override def productUniversality(o: O, ms: List[M]) = ???

    override def coproduct(xs: O*) = internalize(new FunctorToSet {
      override def onObjects(o: smallCategory.O) = Sets.coproduct(xs.map(_(o)):_*)
      override def onMorphisms(m: smallCategory.M) = Sets.coproduct(xs.map(_(m)):_*)
    })
    override def coproductInjections(xs: O*) = ???
    override def coproductUniversality(o: O, ms: List[M]) = ???

  }

  protected[category] class SpecializedFunctorsToSet extends FunctorsToSet { functorsToSet =>
    override type O = smallCategory.F
    override type M = smallCategory.T
  }

  object AllFunctorsToSet extends FunctorsToSet {
    override type O = smallCategory.FunctorToSet
    override type M = smallCategory.NaturalTransformationToSet
  }

  def functorsToSet: SpecializedFunctorsToSet

  protected trait CategoryOver extends functor.withSmallSource.withSmallTarget { categoryOver =>
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
  protected trait NaturalTransformationOver extends NaturalTransformation {
    override val source: CategoryOver
    override val target: CategoryOver
  }

  protected trait FunctorOver { functorOver =>
    val source: CategoryOver
    val target: CategoryOver
    trait F extends Functor {
      override val source: functorOver.source.source.type = functorOver.source.source
      override val target: functorOver.target.source.type = functorOver.target.source
    }
    def functor: F
  }

  protected trait CategoriesOver extends Category { categoriesOver =>
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

  protected trait Opposite extends Category {
    type O = smallCategory.O
    type M = smallCategory.M
    override def identity(o: smallCategory.O) = smallCategory.identity(o)
    override def source(m: smallCategory.M) = smallCategory.target(m)
    override def target(m: smallCategory.M) = smallCategory.source(m)
    override def compose(m1: smallCategory.M, m2: smallCategory.M) = smallCategory.compose(m2, m1)
  }

  val opposite: SmallCategory

}