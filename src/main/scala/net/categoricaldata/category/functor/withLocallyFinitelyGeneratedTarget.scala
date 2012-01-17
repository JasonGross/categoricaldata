package net.categoricaldata.category.functor
import net.categoricaldata.category._

trait withLocallyFinitelyGeneratedTarget extends withSmallTarget {
  override val target: LocallyFinitelyGeneratedCategory
}
