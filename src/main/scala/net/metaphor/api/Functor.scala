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
    trait withSmallTarget extends Functor.withSmallSource with Functor.withSmallTarget { smallFunctor =>
      trait ContravariantDataFunctor extends Functor {
        override val source = smallFunctor.target.AllFunctorsToSet
        override val target: smallFunctor.source.SpecializedFunctorsToSet = smallFunctor.source.functorsToSet
        def andThen(g: CovariantDataFunctor) = DataFunctors.compose(this, g)
      }
      trait CovariantDataFunctor extends Functor {
        override val source = smallFunctor.source.AllFunctorsToSet
        override val target: smallFunctor.target.SpecializedFunctorsToSet = smallFunctor.target.functorsToSet
        def andThen(g: ContravariantDataFunctor) = DataFunctors.compose(this, g)
      }

      object DataFunctors {
        class TargetComposition(f: ContravariantDataFunctor, g: CovariantDataFunctor) extends Functor {
          override val source: f.source.type = f.source
          override val target: g.target.type = g.target
          override def onObjects(o: source.O): target.O = g(f(o))
          override def onMorphisms(m: source.M): target.M = g(f(m))
        }
        class SourceComposition(f: CovariantDataFunctor, g: ContravariantDataFunctor) extends Functor {
          override val source: f.source.type = f.source
          override val target: g.target.type = g.target
          override def onObjects(o: source.O): target.O = g(f(o))
          override def onMorphisms(m: source.M): target.M = g(f(m))
        }

        def compose(f: ContravariantDataFunctor, g: CovariantDataFunctor) = new TargetComposition(f, g)
        def compose(f: CovariantDataFunctor, g: ContravariantDataFunctor) = new SourceComposition(f, g)
      }

      trait Pullback extends ContravariantDataFunctor {
        override def onObjects(i: smallFunctor.target.FunctorToSet) = smallFunctor.source.internalize(new smallFunctor.source.FunctorToSet {
          def onObjects(o: smallFunctor.source.O) = i(smallFunctor.apply(o))
          def onMorphisms(m: smallFunctor.source.M) = i(smallFunctor.apply(m))
        })
        override def onMorphisms(m: smallFunctor.target.NaturalTransformationToSet) = smallFunctor.source.internalize(new smallFunctor.source.NaturalTransformationToSet {
          val source = onObjects(m.target)
          val target = onObjects(m.source)
          def apply(o: smallFunctor.source.O) = m(smallFunctor.apply(o))
        })
      }

      lazy val pullback = new Pullback {}

      lazy val ^* = new Functor {
        override val source = FunctorsToSet
        override val target = smallFunctor.source.functorsToSet
        def onObjects(i: FunctorToSet) = pullback.apply(smallFunctor.target.internalize(i))
        def onMorphisms(t: NaturalTransformationToSet) = pullback.apply(smallFunctor.target.internalize(t))
      }

    }
    trait withLocallyFinitelyGeneratedTarget extends Functor.withSmallSource.withSmallTarget with Functor.withLocallyFinitelyGeneratedTarget
    trait withFinitelyGeneratedTarget extends Functor.withSmallSource.withLocallyFinitelyGeneratedTarget with Functor.withFinitelyGeneratedTarget
    trait withFinitelyPresentedTarget extends Functor.withSmallSource.withFinitelyGeneratedTarget with Functor.withFinitelyPresentedTarget
  }
  object withLocallyFinitelyGeneratedSource {
    trait withSmallTarget extends Functor.withSmallSource.withSmallTarget with Functor.withLocallyFinitelyGeneratedSource
    trait withLocallyFinitelyGeneratedTarget extends Functor.withSmallSource.withLocallyFinitelyGeneratedTarget with Functor.withLocallyFinitelyGeneratedSource
    trait withFinitelyGeneratedTarget extends Functor.withSmallSource.withFinitelyGeneratedTarget with Functor.withLocallyFinitelyGeneratedSource
    trait withFinitelyPresentedTarget extends Functor.withSmallSource.withFinitelyPresentedTarget with Functor.withLocallyFinitelyGeneratedSource
  }
  object withFinitelyGeneratedSource {
    trait withSmallTarget extends Functor.withLocallyFinitelyGeneratedSource.withSmallTarget with Functor.withFinitelyGeneratedSource
    trait withLocallyFinitelyGeneratedTarget extends Functor.withLocallyFinitelyGeneratedSource.withLocallyFinitelyGeneratedTarget with Functor.withFinitelyGeneratedSource
    trait withFinitelyGeneratedTarget extends Functor.withLocallyFinitelyGeneratedSource.withFinitelyGeneratedTarget with Functor.withFinitelyGeneratedSource
    trait withFinitelyPresentedTarget extends Functor.withLocallyFinitelyGeneratedSource.withFinitelyPresentedTarget with Functor.withFinitelyGeneratedSource
  }
  object withFinitelyPresentedSource {
    trait withSmallTarget extends Functor.withFinitelyGeneratedSource.withSmallTarget with Functor.withFinitelyPresentedSource
    trait withLocallyFinitelyGeneratedTarget extends Functor.withFinitelyGeneratedSource.withLocallyFinitelyGeneratedTarget with Functor.withFinitelyPresentedSource
    trait withFinitelyGeneratedTarget extends Functor.withFinitelyGeneratedSource.withFinitelyGeneratedTarget with Functor.withFinitelyPresentedSource
    trait withFinitelyPresentedTarget extends Functor.withFinitelyGeneratedSource.withFinitelyPresentedTarget with Functor.withFinitelyPresentedSource
  }
}


trait MemoFunctor extends Functor {
  import net.tqft.toolkit.functions.Memo
  val memoOnObjects = Memo(super.onObjects _)
  val memoOnMorphisms = Memo(super.onMorphisms _)
  abstract override def onObjects(o: source.O) = memoOnObjects(o)
  abstract override def onMorphisms(o: source.M) = memoOnMorphisms(o)
}
