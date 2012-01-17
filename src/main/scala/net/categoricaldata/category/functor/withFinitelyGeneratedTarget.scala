package net.categoricaldata.category.functor
import net.categoricaldata.category._

trait withFinitelyGeneratedTarget extends withLocallyFinitelyGeneratedTarget {
  override val target: FinitelyGeneratedCategory
}
