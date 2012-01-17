package net.categoricaldata.category.functor
import net.categoricaldata.category._

trait withFinitelyGeneratedSource extends withLocallyFinitelyGeneratedSource {
  override val source: FinitelyGeneratedCategory
}
