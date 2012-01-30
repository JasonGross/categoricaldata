package net.categoricaldata.category.functor.withFinitelyGeneratedSource
import net.categoricaldata.category._

trait withLocallyFinitelyGeneratedTarget extends functor.withLocallyFinitelyGeneratedSource.withLocallyFinitelyGeneratedTarget with functor.withFinitelyGeneratedSource.withSmallTarget { functor =>
  class SliceCategory(maximumPathLength: Int, onLeft: functor.target.O) extends super.SliceCategory(onLeft) {
    override val maximumLevel: Int = functor.source.maximumLevel + maximumPathLength
  }
  class CosliceCategory(maximumPathLength: Int, onRight: functor.target.O) extends super.CosliceCategory(onRight) {
    override val maximumLevel: Int = functor.source.maximumLevel + maximumPathLength
  }
}
