package net.categoricaldata.category.functor.withFinitelyGeneratedSource
import net.categoricaldata.category._

trait withSmallTarget extends functor.withLocallyFinitelyGeneratedSource.withSmallTarget with functor.withFinitelyGeneratedSource
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
trait withFinitelyGeneratedTarget extends functor.withLocallyFinitelyGeneratedSource.withFinitelyGeneratedTarget with functor.withFinitelyGeneratedSource.withLocallyFinitelyGeneratedTarget { fgFunctor =>
  abstract class SliceFunctor extends Functor { sliceFunctor => // D^op --> Cat_{/C}
    override val source: fgfunctor.target.opposite.type = fgfunctor.target.opposite
    override val target: fgfunctor.source.FinitelyGeneratedCategoriesOver = fgfunctor.source.finitelyGeneratedCategoriesOver
    override def onObjects(s: source.O): SliceCategoryOver = new SliceCategoryOver(s)
    override def onMorphisms(m: source.M): SliceFunctorOver = new SliceFunctorOver(m)

    class SliceCategoryOver(onLeft: fgfunctor.target.opposite.O) extends fgfunctor.source.FinitelyGeneratedCategoryOver { // (d | F) --> C
      override val source = buildSliceCategory(onLeft)
      override def onObjects(o: source.ObjectRightOf) = o.right
      override def onGenerators(g: source.ObjectRightOfMap) = {
        import fgfunctor.source.generatorAsMorphism
        g.generator
      }
    }
    class SliceFunctorOver(m: fgfunctor.target.opposite.M) extends fgfunctor.source.FinitelyGeneratedFunctorOver { // d --> d' ~> (d' | F) --> (d | F)
      override val source = slicefunctor.onObjects(fgfunctor.target.opposite.source(m))
      override val target = slicefunctor.onObjects(fgfunctor.target.opposite.target(m))
      override val functor = new F {
        override def onObjects(o: source.ObjectRightOf): target.ObjectRightOf = {
          target.ObjectRightOf(right = o.right, morphism = fgfunctor.target.compose(fgfunctor.target.opposite.unreverse(m), o.morphism))
        }
        override def onGenerators(g: source.ObjectRightOfMap): target.M = {
          target.generatorAsMorphism(target.ObjectRightOfMap(onObjects(g.source), onObjects(g.target), g.generator))
        }
      }
    }

    def buildSliceCategory: fgfunctor.target.O => SliceCategory
  }

  abstract class CosliceFunctor extends Functor { cosliceFunctor => // D --> Cat_{/C}
    override val source: fgfunctor.target.type = fgfunctor.target
    override val target: fgfunctor.source.FinitelyGeneratedCategoriesOver = fgfunctor.source.finitelyGeneratedCategoriesOver
    override def onObjects(s: source.O): CosliceCategoryOver = new CosliceCategoryOver(s)
    override def onMorphisms(m: source.M): CosliceFunctorOver = new CosliceFunctorOver(m)

    class CosliceCategoryOver(onRight: fgfunctor.target.O) extends fgfunctor.source.FinitelyGeneratedCategoryOver { // (F | d) --> C
      override val source = buildCosliceCategory(onRight)
      override def onObjects(o: source.ObjectLeftOf) = o.left
      override def onGenerators(g: source.ObjectLeftOfMap) = {
        import fgfunctor.source.generatorAsMorphism
        g.generator
      }
    }

    class CosliceFunctorOver(m: fgfunctor.target.M) extends fgfunctor.source.FinitelyGeneratedFunctorOver { // d --> d' ~> (F | d) --> (F | d')
      override val source = coslicefunctor.onObjects(fgfunctor.target.source(m))
      override val target = coslicefunctor.onObjects(fgfunctor.target.target(m))
      override val functor = new F {
        override def onObjects(o: source.ObjectLeftOf): target.ObjectLeftOf = {
          target.ObjectLeftOf(left = o.left, morphism = fgfunctor.target.compose(o.morphism, m))
        }
        override def onGenerators(g: source.ObjectLeftOfMap): target.M = {
          target.generatorAsMorphism(target.ObjectLeftOfMap(onObjects(g.target), onObjects(g.source), g.generator))
        }
      }
    }

    def buildCosliceCategory: fgfunctor.target.opposite.O => CosliceCategory
  }

  trait Pullback extends super.Pullback {
    def limitMorphism(i: fgfunctor.target.FunctorToSet) = FFunction(
      source = onObjects(i).limitSet,
      target = i.limitSet,
      function = { x: Map[fgfunctor.source.O, Any] => ??? /* TODO build something, via the universal property */ })
  }

  override lazy val pullback = new Pullback {}
}
trait withFinitelyPresentedTarget extends functor.withLocallyFinitelyGeneratedSource.withFinitelyPresentedTarget with functor.withFinitelyGeneratedSource.withFinitelyGeneratedTarget
