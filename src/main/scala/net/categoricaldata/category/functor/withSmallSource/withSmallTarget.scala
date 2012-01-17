package net.categoricaldata.category.functor.withSmallSource
import net.categoricaldata.category._

trait withSmallTarget extends functor.withSmallSource with functor.withSmallTarget { smallFunctor =>
  trait ContravariantDataFunctor extends Functor {
    override val source = smallfunctor.target.AllFunctorsToSet
    override val target: smallfunctor.source.SpecializedFunctorsToSet = smallfunctor.source.functorsToSet
    def andThen(g: CovariantDataFunctor) = DataFunctors.compose(this, g)
  }
  trait CovariantDataFunctor extends Functor {
    override val source = smallfunctor.source.AllFunctorsToSet
    override val target: smallfunctor.target.SpecializedFunctorsToSet = smallfunctor.target.functorsToSet
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
    override def onObjects(i: smallfunctor.target.FunctorToSet) = smallfunctor.source.internalize(new smallfunctor.source.FunctorToSet {
      def onObjects(o: smallfunctor.source.O) = i(smallfunctor.apply(o))
      def onMorphisms(m: smallfunctor.source.M) = i(smallfunctor.apply(m))
    })
    override def onMorphisms(m: smallfunctor.target.NaturalTransformationToSet) = smallfunctor.source.internalize(new smallfunctor.source.NaturalTransformationToSet {
      val source = onObjects(m.target)
      val target = onObjects(m.source)
      def apply(o: smallfunctor.source.O) = m(smallfunctor.apply(o))
    })

    // c.f. functor.withFinitelyGeneratedSource.withFinitelyGeneratedTarget.Pullback, which adds limitMorphism
  }

  lazy val pullback = new Pullback {}

  lazy val ^* = new Functor {
    override val source = FunctorsToSet
    override val target = smallfunctor.source.functorsToSet
    def onObjects(i: FunctorToSet) = pullback.apply(smallfunctor.target.internalize(i))
    def onMorphisms(t: NaturalTransformationToSet) = pullback.apply(smallfunctor.target.internalize(t))
  }

}
trait withLocallyFinitelyGeneratedTarget extends functor.withSmallSource.withSmallTarget with functor.withLocallyFinitelyGeneratedTarget
trait withFinitelyGeneratedTarget extends functor.withSmallSource.withLocallyFinitelyGeneratedTarget with functor.withFinitelyGeneratedTarget
trait withFinitelyPresentedTarget extends functor.withSmallSource.withFinitelyGeneratedTarget with functor.withFinitelyPresentedTarget
