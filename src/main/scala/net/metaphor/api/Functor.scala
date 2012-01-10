package net.metaphor.api

trait Functor {
  val source: Category
  val target: Category

  final def apply(o: source.O): target.O = onObjects(o)
  // the dummy implicit argument is a hack to allow overloading of apply
  final def apply(m: source.M)(implicit d: DummyImplicit): target.M = onMorphisms(m)

  def onObjects(o: source.O): target.O
  def onMorphisms(m: source.M): target.M
}

object Functor {
  class IdentityFunctor(val category: Category) extends Functor {
    val source: category.type = category
    val target: category.type = category
    def onObjects(o: category.O) = o
    def onMorphisms(m: category.M) = m
  }

  class CompositeFunctor(val functor1: Functor, val functor2: Functor) extends Functor {
    require(functor1.target == functor2.source)
    val source: functor1.source.type = functor1.source
    val target: functor2.target.type = functor2.target
    def onObjects(o: source.O) = functor2(functor1(o).asInstanceOf[functor2.source.O])
    def onMorphisms(m: source.M) = functor2(functor1(m).asInstanceOf[functor2.source.M])
  }

  trait withSmallSource extends Functor {
    override val source: SmallCategory
  }
  trait withSmallTarget extends Functor {
    override val target: SmallCategory
  }

  trait withLocallyFinitelyGeneratedSource extends withSmallSource {
    override val source: LocallyFinitelyGeneratedCategory
    def onGenerators(g: source.G): target.M
    override def onMorphisms(m: source.M) = {
      val start = onObjects(source.source(m))
      val morphisms = for (g <- m.representative.morphisms) yield onGenerators(g)
      target.compose(start, morphisms)
    }
  }
  trait withLocallyFinitelyGeneratedTarget extends withSmallTarget {
    override val target: LocallyFinitelyGeneratedCategory
  }

  trait withFinitelyGeneratedSource extends withLocallyFinitelyGeneratedSource {
    override val source: FinitelyGeneratedCategory
  }
  trait withFinitelyGeneratedTarget extends withLocallyFinitelyGeneratedTarget {
    override val target: FinitelyGeneratedCategory
  }

  trait withFinitelyPresentedSource extends withFinitelyGeneratedSource {
    override val source: FinitelyPresentedCategory
  }
  trait withFinitelyPresentedTarget extends withFinitelyGeneratedTarget {
    override val target: FinitelyPresentedCategory
  }

  object withSmallSource {
    trait withSmallTarget extends Functor.withSmallSource with Functor.withSmallTarget
    trait withLocallyFinitelyGeneratedTarget extends Functor.withSmallSource with Functor.withLocallyFinitelyGeneratedTarget
    trait withFinitelyGeneratedTarget extends Functor.withSmallSource with Functor.withFinitelyGeneratedTarget
    trait withFinitelyPresentedTarget extends Functor.withSmallSource with Functor.withFinitelyPresentedTarget
  }
  object withLocallyFinitelyGeneratedSource {
    trait withSmallTarget extends Functor.withLocallyFinitelyGeneratedSource with Functor.withSmallTarget
    trait withLocallyFinitelyGeneratedTarget extends Functor.withLocallyFinitelyGeneratedSource with Functor.withLocallyFinitelyGeneratedTarget
    trait withFinitelyGeneratedTarget extends Functor.withLocallyFinitelyGeneratedSource with Functor.withFinitelyGeneratedTarget
    trait withFinitelyPresentedTarget extends Functor.withLocallyFinitelyGeneratedSource with Functor.withFinitelyPresentedTarget
  }
  object withFinitelyGeneratedSource {
    trait withSmallTarget extends Functor.withFinitelyGeneratedSource with Functor.withSmallTarget
    trait withLocallyFinitelyGeneratedTarget extends Functor.withFinitelyGeneratedSource with Functor.withLocallyFinitelyGeneratedTarget
    trait withFinitelyGeneratedTarget extends Functor.withFinitelyGeneratedSource with Functor.withFinitelyGeneratedTarget
    trait withFinitelyPresentedTarget extends Functor.withFinitelyGeneratedSource with Functor.withFinitelyPresentedTarget
  }
  object withFinitelyPresentedSource {
    trait withSmallTarget extends Functor.withFinitelyPresentedSource with Functor.withSmallTarget
    trait withLocallyFinitelyGeneratedTarget extends Functor.withFinitelyPresentedSource with Functor.withLocallyFinitelyGeneratedTarget
    trait withFinitelyGeneratedTarget extends Functor.withFinitelyPresentedSource with Functor.withFinitelyGeneratedTarget
    trait withFinitelyPresentedTarget extends Functor.withFinitelyPresentedSource with Functor.withFinitelyPresentedTarget
  }

}

trait LeftAdjoint { functor: Functor =>
  def rightAdjoint: RightAdjoint
  def rightUnit: NaturalTransformation
  def rightCounit: NaturalTransformation
}

trait RightAdjoint { functor: Functor =>
  def leftAdjoint: LeftAdjoint
  def leftUnit: NaturalTransformation
  def leftCounit: NaturalTransformation
}

trait MemoFunctor extends Functor {
  import net.tqft.toolkit.functions.Memo
  val memoOnObjects = Memo(super.onObjects _)
  val memoOnMorphisms = Memo(super.onMorphisms _)
  abstract override def onObjects(o: source.O) = memoOnObjects(o)
  abstract override def onMorphisms(o: source.M) = memoOnMorphisms(o)
}
