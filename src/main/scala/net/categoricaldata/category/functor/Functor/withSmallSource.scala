package net.categoricaldata.category.functor
import net.categoricaldata.category._

trait withSmallSource extends Functor {
  override val source: SmallCategory
}
