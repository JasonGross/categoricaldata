package net.metaphor.api

trait FunctorWithFinitelyGeneratedSource extends LocallyFinitelyGeneratedFunctor {
  override val source: FinitelyGeneratedCategory  
}

trait FinitelyGeneratedFunctor extends LocallyFinitelyGeneratedFunctorWithLocallyFinitelyGeneratedTarget { fgFunctor =>

  override val source: FinitelyGeneratedCategory
  override val target: FinitelyGeneratedCategory

  class SliceCategory(maximumPathLength: Int, onLeft: fgFunctor.target.O) extends super.SliceCategory(onLeft) {
    override val maximumLevel: Int = fgFunctor.source.maximumLevel + maximumPathLength
    
    trait FunctorToSet extends super.FunctorToSet {
      // ACHTUNG this might be an expensive test:
      for(o1 <- objects; o2 <- objects; if o1 == o2) {
        require(onObjects(o1) == onObjects(o2))
      }
    }
    
    override def internalize(f: net.metaphor.api.FunctorToSet) = new FunctorToSet {
      require(f.source == source)
      override def onObjects(o: O) = f.onObjects(o.asInstanceOf[f.source.O])
      override def onGenerators(g: G) = f.onMorphisms(generatorAsMorphism(g).asInstanceOf[f.source.M])
    }
  }
  class CosliceCategory(maximumPathLength: Int, onRight: fgFunctor.target.O) extends super.CosliceCategory(onRight) {
    override val maximumLevel: Int = fgFunctor.source.maximumLevel + maximumPathLength

    trait FunctorToSet extends super.FunctorToSet {
      // ACHTUNG this might be an expensive test:
      for(o1 <- objects; o2 <- objects; if o1 == o2) {
        require(onObjects(o1) == onObjects(o2))
      }
    }
    
    override def internalize(f: net.metaphor.api.FunctorToSet) = new FunctorToSet {
      require(f.source == source)
      override def onObjects(o: O) = f.onObjects(o.asInstanceOf[f.source.O])
      override def onGenerators(g: G) = f.onMorphisms(generatorAsMorphism(g).asInstanceOf[f.source.M])
    }
  }

  abstract class SliceFunctor extends Functor { sliceFunctor => // D^op --> Cat_{/C}
    override val source: fgFunctor.target.opposite.type = fgFunctor.target.opposite
    override val target: fgFunctor.source.FinitelyGeneratedCategoriesOver = fgFunctor.source.finitelyGeneratedCategoriesOver
    override def onObjects(s: source.O): SliceCategoryOver = new SliceCategoryOver(s)
    override def onMorphisms(m: source.M): SliceFunctorOver = new SliceFunctorOver(m)

    class SliceCategoryOver(onLeft: fgFunctor.target.opposite.O) extends fgFunctor.source.FinitelyGeneratedCategoryOver { // (d | F) --> C
      override val source = getSliceCategory(onLeft)
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

    //    val getSliceCategory = net.tqft.toolkit.functions.Memo(buildSliceCategory _)
    val getSliceCategory = buildSliceCategory _
    def buildSliceCategory(onLeft: fgFunctor.target.O): SliceCategory
  }

  abstract class CosliceFunctor extends Functor { cosliceFunctor => // D --> Cat_{/C}
    override val source: fgFunctor.target.type = fgFunctor.target
    override val target: fgFunctor.source.FinitelyGeneratedCategoriesOver = fgFunctor.source.finitelyGeneratedCategoriesOver
    override def onObjects(s: source.O): CosliceCategoryOver = new CosliceCategoryOver(s)
    override def onMorphisms(m: source.M): CosliceFunctorOver = new CosliceFunctorOver(m)

    class CosliceCategoryOver(onRight: fgFunctor.target.O) extends fgFunctor.source.FinitelyGeneratedCategoryOver { // (F | d) --> C
      override val source = getCosliceCategory(onRight)
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

    //    val getCosliceCategory = net.tqft.toolkit.functions.Memo(buildCosliceCategory _)
    val getCosliceCategory = buildCosliceCategory _
    def buildCosliceCategory(onLeft: fgFunctor.target.opposite.O): CosliceCategory
  }
}