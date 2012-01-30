package net.categoricaldata.category.functor.withFinitelyPresentedSource
import net.categoricaldata.category._

trait withFinitelyGeneratedTarget extends functor.withFinitelyGeneratedSource.withFinitelyGeneratedTarget with functor.withFinitelyPresentedSource.withLocallyFinitelyGeneratedTarget {
  // the Pullback hierarchy had diverged; we reunite it here!
  trait Pullback extends super.Pullback with super[withFinitelyGeneratedTarget].Pullback
}
