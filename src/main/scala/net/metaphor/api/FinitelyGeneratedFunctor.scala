package net.metaphor.api

trait FinitelyGeneratedFunctor[C <: FinitelyGeneratedCategory[C]] extends SmallFunctor[C] with FunctorWithFinitelyGeneratedSource[C, C] { fgFunctor =>
  type SC <: SliceCategory
  type cSC <: CosliceCategory

  class SliceCategory(onRight: fgFunctor.target.O) extends FinitelyGeneratedCategory[SC] { sliceCategory: SC =>
    override type O = ObjectLeftOf
    override type G = ObjectLeftOfMap

    def generatorSource(g: G) = g.source
    def generatorTarget(g: G) = g.target

    case class ObjectLeftOf(left: fgFunctor.source.O, morphism: fgFunctor.target.M) {
      require(fgFunctor.target.source(morphism) == fgFunctor.apply(left))
      require(fgFunctor.target.target(morphism) == onRight)
    }
    case class ObjectLeftOfMap(source: ObjectLeftOf, target: ObjectLeftOf, generator: fgFunctor.source.G) {
      require(fgFunctor.source.generatorSource(generator) == source.left)
      require(fgFunctor.source.generatorTarget(generator) == target.left)
      require(fgFunctor.target.compose(fgFunctor.onGenerators(generator), target.morphism) == source.morphism)
    }

    def opposite = ??? // new SliceCategory(onRight) with Opposite
    def objectsAtLevel(k: Int): List[ObjectLeftOf] = {
      for (
        l <- (fgFunctor.source.minimumLevel to k).toList;
        left <- fgFunctor.source.objectsAtLevel(l);
        path <- fgFunctor.target.wordsOfLength(k - l)(fgFunctor.apply(left), onRight)
      ) yield ObjectLeftOf(left, fgFunctor.target.pathAsMorphism(path))
    }
    val minimumLevel: Int = fgFunctor.source.minimumLevel
    val maximumLevel: Int = 100 // FIXME fgFunctor.source.maximumLevel + fgFunctor.target.maximumWordLength

    def generators(source: ObjectLeftOf, target: ObjectLeftOf): List[ObjectLeftOfMap] = {
      import fgFunctor.source.generatorAsMorphism
      for (g <- fgFunctor.source.generators(source.left, target.left); if fgFunctor.target.compose(fgFunctor.apply(g), target.morphism) == source.morphism) yield {
        ObjectLeftOfMap(source, target, g)
      }
    }

    type CSets = FunctorsToSet
    val functorsToSet = new FunctorsToSet
    type F = FunctorToSet
    type T = NaturalTransformationToSet[FunctorToSet]

    override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[SC]): FunctorToSet = {
      require(f.source == sliceCategory)
      new FunctorToSet {
        override def onObjects(o: source.O) = f(o.asInstanceOf[f.source.O])
        override def onGenerators(g: source.G) = f(f.source.generatorAsMorphism(g.asInstanceOf[f.source.G]))
      }
    }
    override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[SC, F]): NaturalTransformationToSet[FunctorToSet] = {
      require(t.sourceCategory == fgFunctor.source)
      new NaturalTransformationToSet[FunctorToSet] {
        override val source = t.source
        override val target = t.target
        override def apply(o: sourceCategory.O) = t(o)
      }
    }

    lazy val adjoinInitialObject = ???
    //      FIXME, get something like this to work?
    //      new ConcreteFinitelyGeneratedCategory with WithInitialObject[ConcreteFinitelyGeneratedCategory] {
    //      val initialObject = ???
    //      def morphismTo(o: O) = ???
    //    }
    lazy val adjoinTerminalObject = ??? // probably never used

  }

  class CosliceCategory(onLeft: fgFunctor.target.O) extends FinitelyGeneratedCategory[cSC] { cosliceCategory: cSC =>
    override type O = ObjectRightOf
    override type G = ObjectRightOfMap

    def generatorSource(g: G) = g.source
    def generatorTarget(g: G) = g.target

    case class ObjectRightOf(right: fgFunctor.source.O, morphism: fgFunctor.target.M) {
      require(fgFunctor.target.source(morphism) == onLeft)
      require(fgFunctor.target.target(morphism) == fgFunctor.apply(right))
    }
    case class ObjectRightOfMap(source: ObjectRightOf, target: ObjectRightOf, generator: fgFunctor.source.G) {
      require(fgFunctor.source.generatorSource(generator) == source.right)
      require(fgFunctor.source.generatorTarget(generator) == target.right)
      require(fgFunctor.target.compose(source.morphism, fgFunctor.onGenerators(generator)) == target.morphism)
    }

    def opposite = ??? // new CosliceCategory(onLeft) with Opposite
    def objectsAtLevel(k: Int): List[ObjectRightOf] = {
      for (
        l <- (fgFunctor.source.minimumLevel to k).toList;
        right <- fgFunctor.source.objectsAtLevel(l);
        path <- fgFunctor.target.wordsOfLength(k - l)(onLeft, fgFunctor.apply(right))
      ) yield ObjectRightOf(right, fgFunctor.target.pathAsMorphism(path))
    }
    val minimumLevel: Int = fgFunctor.source.minimumLevel
    val maximumLevel: Int = 100 // FIXME fgFunctor.source.maximumLevel + fgFunctor.target.maximumWordLength
    def generators(source: ObjectRightOf, target: ObjectRightOf): List[ObjectRightOfMap] = {
      import fgFunctor.source.generatorAsMorphism
      for (g <- fgFunctor.source.generators(source.right, target.right); if fgFunctor.target.compose(source.morphism, fgFunctor.apply(g)) == target.morphism) yield {
        ObjectRightOfMap(source, target, g)
      }
    }

    type CSets = FunctorsToSet
    val functorsToSet = new FunctorsToSet
    type F = FunctorToSet
    type T = NaturalTransformationToSet[FunctorToSet]

    override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[cSC]): FunctorToSet = new FunctorToSet {
      require(f.source == cosliceCategory)
      override def onObjects(o: source.O) = f(o.asInstanceOf[f.source.O])
      override def onGenerators(g: source.G) = f(g.asInstanceOf[f.source.M])
    }
    override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[cSC, F]): NaturalTransformationToSet[FunctorToSet] = new NaturalTransformationToSet[FunctorToSet] {
      override val source = t.source
      override val target = t.target
      override def apply(o: sourceCategory.O) = t(o)
    }

    lazy val adjoinInitialObject = ??? // probably never used
    lazy val adjoinTerminalObject = ???
  }

  abstract class SliceFunctor extends HeteroFunctor[C, source.CategoriesOver[SC]] { sliceFunctor =>
    override val source: fgFunctor.target.type = fgFunctor.target
    override val target = fgFunctor.source.categoriesOver[SC]
    override def onObjects(s: source.O): target.O = new SliceCategoryOver(s)
    override def onMorphisms(m: source.M): target.M = new SliceFunctorOver(m)

    class SliceCategoryOver(onRight: fgFunctor.target.O) extends fgFunctor.source.CategoryOver[SC] {
      override val functor: fgFunctor.source.FunctorTo[SC] = new fgFunctor.source.FinitelyGeneratedFunctorTo[SC] {
        override val source = buildSliceCategory(onRight)
        override def onObjects(o: source.ObjectLeftOf) = o.left
        override def onGenerators(g: source.ObjectLeftOfMap) = {
          import fgFunctor.source.generatorAsMorphism
          g.generator
        }
      }
    }
    class SliceFunctorOver(m: fgFunctor.target.M) extends fgFunctor.source.FunctorOver[SC] {
      override def source = onObjects(fgFunctor.target.source(m))
      override def target = onObjects(fgFunctor.target.target(m))
      override def functor = ???
    }

    def buildSliceCategory(onRight: fgFunctor.target.O): SC

  }

  abstract class CosliceFunctor extends HeteroFunctor[C, source.CategoriesOver[cSC]] { cosliceFunctor =>
    override val source: fgFunctor.target.type = fgFunctor.target
    override val target = fgFunctor.source.categoriesOver[cSC]
    override def onObjects(s: source.O): target.O = new CosliceCategoryOver(s)
    override def onMorphisms(m: source.M): target.M = new CosliceFunctorOver(m)

    class CosliceCategoryOver(onLeft: fgFunctor.target.O) extends fgFunctor.source.CategoryOver[cSC] {
      override val functor: fgFunctor.source.FunctorTo[cSC] = new fgFunctor.source.FinitelyGeneratedFunctorTo[cSC] {
        override val source = buildCosliceCategory(onLeft)
        override def onObjects(o: source.ObjectRightOf) = o.right
        override def onGenerators(g: source.ObjectRightOfMap) = {
          import fgFunctor.source.generatorAsMorphism
          g.generator
        }
      }
    }
    class CosliceFunctorOver(m: fgFunctor.target.M) extends fgFunctor.source.FunctorOver[cSC] {
      override def source = onObjects(fgFunctor.target.source(m))
      override def target = onObjects(fgFunctor.target.target(m))
      override def functor = ???
    }

    def buildCosliceCategory(onLeft: fgFunctor.target.O): cSC
  }
}