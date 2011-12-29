package net.metaphor.api

trait FinitelyGeneratedFunctor extends SmallFunctor { fgFunctor =>

  override val source: FinitelyGeneratedCategory
  override val target: FinitelyGeneratedCategory

  def onGenerators(g: source.G): target.M
  override def onMorphisms(m: source.M) = {
    val start = onObjects(source.source(m))
    val morphisms = for (g <- m.representative.morphisms) yield onGenerators(g)
    target.compose(start, morphisms)
  }

  type SC <: SliceCategory
  type cSC <: CosliceCategory

  class SliceCategory(onRight: fgFunctor.target.O) extends FinitelyGeneratedCategory with FinitelyGeneratedCategories.StandardFunctorsToSet { sliceCategory: SC =>
    override type O = ObjectLeftOf
    override type G = ObjectLeftOfMap

    override def generatorSource(g: G) = g.source
    override def generatorTarget(g: G) = g.target

    case class ObjectLeftOf(left: fgFunctor.source.O, morphism: fgFunctor.target.M) {
      require(fgFunctor.target.source(morphism) == fgFunctor.apply(left))
      require(fgFunctor.target.target(morphism) == onRight)
    }
    case class ObjectLeftOfMap(source: ObjectLeftOf, target: ObjectLeftOf, generator: fgFunctor.source.G) {
      require(fgFunctor.source.generatorSource(generator) == source.left)
      require(fgFunctor.source.generatorTarget(generator) == target.left)
      require(fgFunctor.target.compose(fgFunctor.onGenerators(generator), target.morphism) == source.morphism)
    }

    override def objectsAtLevel(k: Int): List[ObjectLeftOf] = {
      for (
        l <- (fgFunctor.source.minimumLevel to k).toList;
        left <- fgFunctor.source.objectsAtLevel(l);
        path <- fgFunctor.target.wordsOfLength(k - l)(fgFunctor.apply(left), onRight)
      ) yield ObjectLeftOf(left, fgFunctor.target.pathAsMorphism(path))
    }
    override val minimumLevel: Int = fgFunctor.source.minimumLevel
    override val maximumLevel: Int = 100 // FIXME fgFunctor.source.maximumLevel + fgFunctor.target.maximumWordLength

    override def generators(source: ObjectLeftOf, target: ObjectLeftOf): List[ObjectLeftOfMap] = {
      import fgFunctor.source.generatorAsMorphism
      for (g <- fgFunctor.source.generators(source.left, target.left); if fgFunctor.target.compose(fgFunctor.apply(g), target.morphism) == source.morphism) yield {
        ObjectLeftOfMap(source, target, g)
      }
    }

  }

  class CosliceCategory(onLeft: fgFunctor.target.O) extends FinitelyGeneratedCategory with FinitelyGeneratedCategories.StandardFunctorsToSet { cosliceCategory: cSC =>
    override type O = ObjectRightOf
    override type G = ObjectRightOfMap

    override def generatorSource(g: G) = g.source
    override def generatorTarget(g: G) = g.target

    case class ObjectRightOf(right: fgFunctor.source.O, morphism: fgFunctor.target.M) {
      require(fgFunctor.target.source(morphism) == onLeft)
      require(fgFunctor.target.target(morphism) == fgFunctor.apply(right))
    }
    case class ObjectRightOfMap(source: ObjectRightOf, target: ObjectRightOf, generator: fgFunctor.source.G) {
      require(fgFunctor.source.generatorSource(generator) == source.right)
      require(fgFunctor.source.generatorTarget(generator) == target.right)
      require(fgFunctor.target.compose(source.morphism, fgFunctor.onGenerators(generator)) == target.morphism)
    }

    override def objectsAtLevel(k: Int): List[ObjectRightOf] = {
      for (
        l <- (fgFunctor.source.minimumLevel to k).toList;
        right <- fgFunctor.source.objectsAtLevel(l);
        path <- fgFunctor.target.wordsOfLength(k - l)(onLeft, fgFunctor.apply(right))
      ) yield ObjectRightOf(right, fgFunctor.target.pathAsMorphism(path))
    }
    override val minimumLevel: Int = fgFunctor.source.minimumLevel
    override val maximumLevel: Int = 100 // FIXME fgFunctor.source.maximumLevel + fgFunctor.target.maximumWordLength
    override def generators(source: ObjectRightOf, target: ObjectRightOf): List[ObjectRightOfMap] = {
      import fgFunctor.source.generatorAsMorphism
      for (g <- fgFunctor.source.generators(source.right, target.right); if fgFunctor.target.compose(source.morphism, fgFunctor.apply(g)) == target.morphism) yield {
        ObjectRightOfMap(source, target, g)
      }
    }

  }

  abstract class SliceFunctor extends Functor { sliceFunctor =>
    override val source: fgFunctor.target.type = fgFunctor.target
    override val target: fgFunctor.source.FinitelyGeneratedCategoriesOver = fgFunctor.source.finitelyGeneratedCategoriesOver
    override def onObjects(s: source.O): SliceCategoryOver = new SliceCategoryOver(s)
    override def onMorphisms(m: source.M): SliceFunctorOver = new SliceFunctorOver(m)

    class SliceCategoryOver(onRight: fgFunctor.target.O) extends fgFunctor.source.FinitelyGeneratedCategoryOver {
      override val source = buildSliceCategory(onRight)
      override def onObjects(o: source.ObjectLeftOf) = o.left
      override def onGenerators(g: source.ObjectLeftOfMap) = {
        import fgFunctor.source.generatorAsMorphism
        g.generator
      }
    }
    class SliceFunctorOver(m: fgFunctor.target.M) extends fgFunctor.source.FinitelyGeneratedFunctorOver {
      override val source = sliceFunctor.onObjects(fgFunctor.target.source(m))
      override val target = sliceFunctor.onObjects(fgFunctor.target.target(m))
      override val functor = new F {
        override def onObjects(o: source.ObjectLeftOf): target.ObjectLeftOf = {
          target.ObjectLeftOf(left = o.left, morphism = fgFunctor.target.compose(o.morphism, m))
        }
        override def onGenerators(g: source.ObjectLeftOfMap): target.M = {
          target.generatorAsMorphism(target.ObjectLeftOfMap(onObjects(g.source), onObjects(g.target), g.generator))
        }
      }
    }

    def buildSliceCategory(onRight: fgFunctor.target.O): SC

  }

  abstract class CosliceFunctor extends Functor { cosliceFunctor =>
    override val source: fgFunctor.target.opposite.type = fgFunctor.target.opposite
    override val target: fgFunctor.source.FinitelyGeneratedCategoriesOver = fgFunctor.source.finitelyGeneratedCategoriesOver
    override def onObjects(s: source.O): CosliceCategoryOver = new CosliceCategoryOver(s)
    override def onMorphisms(m: source.M): CosliceFunctorOver = new CosliceFunctorOver(m)

    class CosliceCategoryOver(onLeft: fgFunctor.target.opposite.O) extends fgFunctor.source.FinitelyGeneratedCategoryOver {
      override val source = buildCosliceCategory(onLeft)
      override def onObjects(o: source.ObjectRightOf) = o.right
      override def onGenerators(g: source.ObjectRightOfMap) = {
        import fgFunctor.source.generatorAsMorphism
        g.generator
      }
    }
    class CosliceFunctorOver(m: fgFunctor.target.opposite.M) extends fgFunctor.source.FinitelyGeneratedFunctorOver {
      override val source = cosliceFunctor.onObjects(fgFunctor.target.opposite.target(m)) // source, target, schmource, schtarget, who's keeping track anyway?
      override val target = cosliceFunctor.onObjects(fgFunctor.target.opposite.source(m))
      override val functor = new F {
        override def onObjects(o: source.ObjectRightOf): target.ObjectRightOf = {
          target.ObjectRightOf(right = o.right, morphism = fgFunctor.target.compose(m.asInstanceOf[fgFunctor.target.M], o.morphism))
        }
        override def onGenerators(g: source.ObjectRightOfMap): target.M = {
          target.generatorAsMorphism(target.ObjectRightOfMap(onObjects(g.target), onObjects(g.source), g.generator))
        }
      }
    }

    def buildCosliceCategory(onLeft: fgFunctor.target.opposite.O): cSC
  }
}