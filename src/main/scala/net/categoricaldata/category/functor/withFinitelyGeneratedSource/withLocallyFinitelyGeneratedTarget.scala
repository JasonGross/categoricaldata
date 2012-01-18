package net.categoricaldata.category.functor.withFinitelyGeneratedSource
import net.categoricaldata.category._

trait withLocallyFinitelyGeneratedTarget extends functor.withLocallyFinitelyGeneratedSource.withLocallyFinitelyGeneratedTarget with functor.withFinitelyGeneratedSource.withSmallTarget { functor =>
  class SliceCategory(maximumPathLength: Int, onLeft: functor.target.O) extends super.SliceCategory(onLeft) {
    override val maximumLevel: Int = functor.source.maximumLevel + maximumPathLength

    override def internalize(f: net.categoricaldata.category.FunctorToSet) = new FunctorToSet {
      require(f.source == source)
      override def onObjects(o: O) = f.onObjects(o.asInstanceOf[f.source.O])
      override def onGenerators(g: G) = f.onMorphisms(generatorAsMorphism(g).asInstanceOf[f.source.M])
    }
  }
  class CosliceCategory(maximumPathLength: Int, onRight: functor.target.O) extends super.CosliceCategory(onRight) {
    override val maximumLevel: Int = functor.source.maximumLevel + maximumPathLength

    override def internalize(f: net.categoricaldata.category.FunctorToSet) = new FunctorToSet {
      require(f.source == source)
      override def onObjects(o: O) = f.onObjects(o.asInstanceOf[f.source.O])
      override def onGenerators(g: G) = f.onMorphisms(generatorAsMorphism(g).asInstanceOf[f.source.M])
    }
  }
}
