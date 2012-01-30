package net.categoricaldata.category.functor.withFinitelyPresentedSource
import net.categoricaldata.category._

trait withSmallTarget extends functor.withFinitelyGeneratedSource.withSmallTarget with Functor.withFinitelyPresentedSource { functor =>
  trait ContravariantDataFunctor extends super.ContravariantDataFunctor {
    override val target: functor.source.FunctorsToSet = functor.source.functorsToSet
  }
  trait Pullback extends super.Pullback with ContravariantDataFunctor
}


