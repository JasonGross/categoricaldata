package net.categoricaldata.category.functor
import net.categoricaldata.category._

trait withSmallTarget extends Functor {
  override val target: SmallCategory
}
