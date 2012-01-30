package net.categoricaldata.category.functor.withFinitelyGeneratedSource
import net.categoricaldata.category._

trait withSmallTarget extends functor.withLocallyFinitelyGeneratedSource.withSmallTarget with Functor.withFinitelyGeneratedSource { functor =>
  trait ContravariantDataFunctor extends super.ContravariantDataFunctor {
    override val target: functor.source.FunctorsToSet = functor.source.functorsToSet
  }
  trait Pullback extends super.Pullback with ContravariantDataFunctor
}


