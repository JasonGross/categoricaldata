package net.categoricaldata.category.functor.withSmallSource
import net.categoricaldata.category._

trait withSmallTarget extends Functor.withSmallSource with Functor.withSmallTarget { smallFunctor =>
  trait ContravariantDataFunctor extends Functor {
    override val source = smallFunctor.target.AllFunctorsToSet
    override val target: smallFunctor.source.FunctorsToSet = smallFunctor.source.functorsToSet
    def andThen(g: CovariantDataFunctor) = DataFunctors.compose(this, g)
  }
  trait CovariantDataFunctor extends Functor {
    override val source = smallFunctor.source.AllFunctorsToSet
    override val target: smallFunctor.target.FunctorsToSet = smallFunctor.target.functorsToSet
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

  trait Pullback extends ContravariantDataFunctor { pullback =>
    override def onObjects(i: smallFunctor.target.FunctorToSet) = pullback.target.internalize(new smallFunctor.source.FunctorToSet {
      def onObjects(o: smallFunctor.source.O) = i(smallFunctor.apply(o))
      def onMorphisms(m: smallFunctor.source.M) = i(smallFunctor.apply(m))
    })
    override def onMorphisms(m: smallFunctor.target.NaturalTransformationToSet) = pullback.target.internalize(new smallFunctor.source.NaturalTransformationToSet {
      val source = onObjects(m.target)
      val target = onObjects(m.source)
      def apply(o: smallFunctor.source.O) = m(smallFunctor.apply(o))
    })

    // c.f. functor.withFinitelyGeneratedSource.withFinitelyGeneratedTarget.Pullback, which adds limitMorphism
  }

  lazy val pullback = new Pullback {}

}
