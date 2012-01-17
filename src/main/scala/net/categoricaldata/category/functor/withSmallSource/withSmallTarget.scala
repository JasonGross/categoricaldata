package net.categoricaldata.category.functor.withSmallSource
import net.categoricaldata.category._

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

    // c.f. functor.withFinitelyGeneratedSource.withFinitelyGeneratedTarget.Pullback, which adds limitMorphism
  }

  lazy val pullback = new Pullback {}

  lazy val ^* = new Functor {
    override val source = FunctorsToSet
    override val target = smallFunctor.source.functorsToSet
    def onObjects(i: FunctorToSet) = pullback.apply(smallFunctor.target.internalize(i))
    def onMorphisms(t: NaturalTransformationToSet) = pullback.apply(smallFunctor.target.internalize(t))
  }

}
trait withLocallyFinitelyGeneratedTarget extends functor.withSmallSource.withSmallTarget with Functor.withLocallyFinitelyGeneratedTarget
trait withFinitelyGeneratedTarget extends functor.withSmallSource.withLocallyFinitelyGeneratedTarget with Functor.withFinitelyGeneratedTarget
trait withFinitelyPresentedTarget extends functor.withSmallSource.withFinitelyGeneratedTarget with Functor.withFinitelyPresentedTarget
