package net.categoricaldata.category.functor
import net.categoricaldata.category._

trait withFinitelyPresentedTarget extends withFinitelyGeneratedTarget {
  override val target: FinitelyPresentedCategory
}
