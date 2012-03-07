package net.categoricaldata.category.functor.withFinitelyGeneratedSource
import net.categoricaldata.category._
import net.categoricaldata.sets.{ FSet, FFunction }

trait withFinitelyGeneratedTarget extends functor.withLocallyFinitelyGeneratedSource.withFinitelyGeneratedTarget with functor.withFinitelyGeneratedSource.withLocallyFinitelyGeneratedTarget { fgFunctor =>
  abstract class SliceFunctor extends Functor { sliceFunctor => // D^op --> Cat_{/C}
    override val source: fgFunctor.target.opposite.type = fgFunctor.target.opposite
    override val target: fgFunctor.source.FinitelyGeneratedCategoriesOver = fgFunctor.source.finitelyGeneratedCategoriesOver
    override def onObjects(s: source.O): SliceCategoryOver = new SliceCategoryOver(s)
    override def onMorphisms(m: source.M): SliceFunctorOver = new SliceFunctorOver(m)

    class SliceCategoryOver(onLeft: fgFunctor.target.opposite.O) extends fgFunctor.source.FinitelyGeneratedCategoryOver { // (d | F) --> C
      override val source = buildSliceCategory(onLeft)
      override def onObjects(o: source.ObjectRightOf) = o.right
      override def onGenerators(g: source.ObjectRightOfMap) = {
        import fgFunctor.source.generatorAsMorphism
        g.generator
      }
    }
    class SliceFunctorOver(m: fgFunctor.target.opposite.M) extends fgFunctor.source.FinitelyGeneratedFunctorOver { // d --> d' ~> (d' | F) --> (d | F)
      override val source = sliceFunctor.onObjects(fgFunctor.target.opposite.source(m))
      override val target = sliceFunctor.onObjects(fgFunctor.target.opposite.target(m))
      override val functor = new F {
        override def onObjects(o: source.ObjectRightOf): target.ObjectRightOf = {
          target.ObjectRightOf(right = o.right, morphism = fgFunctor.target.compose(fgFunctor.target.opposite.unreverse(m), o.morphism))
        }
        override def onGenerators(g: source.ObjectRightOfMap): target.M = {
          target.generatorAsMorphism(target.ObjectRightOfMap(onObjects(g.source), onObjects(g.target), g.generator))
        }
      }
    }

    def buildSliceCategory: fgFunctor.target.O => SliceCategory
  }

  abstract class CosliceFunctor extends Functor { cosliceFunctor => // D --> Cat_{/C}
    override val source: fgFunctor.target.type = fgFunctor.target
    override val target: fgFunctor.source.FinitelyGeneratedCategoriesOver = fgFunctor.source.finitelyGeneratedCategoriesOver
    override def onObjects(s: source.O): CosliceCategoryOver = new CosliceCategoryOver(s)
    override def onMorphisms(m: source.M): CosliceFunctorOver = new CosliceFunctorOver(m)

    class CosliceCategoryOver(onRight: fgFunctor.target.O) extends fgFunctor.source.FinitelyGeneratedCategoryOver { // (F | d) --> C
      override val source = buildCosliceCategory(onRight)
      override def onObjects(o: source.ObjectLeftOf) = o.left
      override def onGenerators(g: source.ObjectLeftOfMap) = {
        import fgFunctor.source.generatorAsMorphism
        g.generator
      }
    }

    class CosliceFunctorOver(m: fgFunctor.target.M) extends fgFunctor.source.FinitelyGeneratedFunctorOver { // d --> d' ~> (F | d) --> (F | d')
      override val source = cosliceFunctor.onObjects(fgFunctor.target.source(m))
      override val target = cosliceFunctor.onObjects(fgFunctor.target.target(m))
      override val functor = new F {
        override def onObjects(o: source.ObjectLeftOf): target.ObjectLeftOf = {
          target.ObjectLeftOf(left = o.left, morphism = fgFunctor.target.compose(o.morphism, m))
        }
        override def onGenerators(g: source.ObjectLeftOfMap): target.M = {
          target.generatorAsMorphism(target.ObjectLeftOfMap(onObjects(g.target), onObjects(g.source), g.generator))
        }
      }
    }

    def buildCosliceCategory: fgFunctor.target.opposite.O => CosliceCategory
  }

  trait Pullback extends super.Pullback {
    def limitMorphism(i: fgFunctor.target.FunctorToSet) = {
      FFunction(
      source = onObjects(i).limitSet,
      target = i.limitSet,
      function = { x: Map[fgFunctor.source.O, Any] => ??? /* TODO build something, via the universal property */ })
    }
    def colimitMorphism(i: fgFunctor.target.FunctorToSet) = {
      FFunction(
      source = i.colimitSet,
      target = onObjects(i).colimitSet,
      function = { x: Set[(fgFunctor.source.O, Any)] => ??? /* TODO build something, via the universal property */ })
    }
  }

  override lazy val pullback = new Pullback {}
}
