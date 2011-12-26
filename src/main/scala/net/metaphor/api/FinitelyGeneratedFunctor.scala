package net.metaphor.api

trait FinitelyGeneratedFunctor[C <: FinitelyGeneratedCategory[C]] extends SmallFunctor[C] { functor =>
  override type SC <: SliceCategory
  override type cSC <: CosliceCategory

  class SliceCategory(onRight: functor.target.O) extends super.SliceCategory(onRight) with FinitelyGeneratedCategory[SC] { sliceCategory: SC =>
    override type O = super.O
    override type M = super.M

    def objectsAtLevel(k: Int): List[ObjectLeftOf] = {
      for (
        l <- (functor.source.minimumLevel to k).toList;
        left <- functor.source.objectsAtLevel(l);
        path <- functor.target.wordsOfLength(k - l)(functor.apply(left), onRight)
      ) yield ObjectLeftOf(left, path)
    }
    val minimumLevel: Int = functor.source.minimumLevel
    val maximumLevel: Int = 100 // FIXME functor.source.maximumLevel + functor.target.maximumWordLength
    def generators(source: ObjectLeftOf, target: ObjectLeftOf): List[ObjectLeftOfMap] = {
      for (g <- functor.source.generators(source.left, target.left); if functor.target.compose(functor.apply(g), target.path) == source.path) yield {
        ObjectLeftOfMap(source, target, g)
      }
    }

    type CSets = FunctorsToSet
    val functorsToSet = new FunctorsToSet
    type F = FunctorToSet
    type T = NaturalTransformationToSet[FunctorToSet]

    override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[SC]): FunctorToSet = new FunctorToSet {
      override def onObjects(o: source.O) = f(o.asInstanceOf[f.source.O])
      override def onMorphisms(m: source.M) = f(m.asInstanceOf[f.source.M])
    }
    override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[SC, F]): NaturalTransformationToSet[FunctorToSet] = new NaturalTransformationToSet[FunctorToSet] {
      override val source = t.source
      override val target = t.target
      override def apply(o: sourceCategory.O) = t(o)
    }

    lazy val adjoinInitialObject = ???
//      FIXME, get something like this to work?
//      new ConcreteFinitelyGeneratedCategory with WithInitialObject[ConcreteFinitelyGeneratedCategory] {
//      val initialObject = ???
//      def morphismTo(o: O) = ???
//    }
    lazy val adjoinTerminalObject = ??? // probably never used

  }

  class CosliceCategory(onLeft: functor.target.O) extends super.CosliceCategory(onLeft) with FinitelyGeneratedCategory[cSC] { cosliceCategory: cSC =>
    override type O = super.O
    override type M = super.M

    def objectsAtLevel(k: Int): List[ObjectRightOf] = {
      for (
        l <- (functor.source.minimumLevel to k).toList;
        right <- functor.source.objectsAtLevel(l);
        path <- functor.target.wordsOfLength(k - l)(onLeft, functor.apply(right))
      ) yield ObjectRightOf(right, path)
    }
    val minimumLevel: Int = functor.source.minimumLevel
    val maximumLevel: Int = 100 // FIXME functor.source.maximumLevel + functor.target.maximumWordLength
    def generators(source: ObjectRightOf, target: ObjectRightOf): List[ObjectRightOfMap] = {
      for (g <- functor.source.generators(source.right, target.right); if functor.target.compose(source.path, functor.apply(g)) == target.path) yield {
        ObjectRightOfMap(source, target, g)
      }
    }

    type CSets = FunctorsToSet
    val functorsToSet = new FunctorsToSet
    type F = FunctorToSet
    type T = NaturalTransformationToSet[FunctorToSet]

    override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[cSC]): FunctorToSet = new FunctorToSet {
      override def onObjects(o: source.O) = f(o.asInstanceOf[f.source.O])
      override def onMorphisms(m: source.M) = f(m.asInstanceOf[f.source.M])
    }
    override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[cSC, F]): NaturalTransformationToSet[FunctorToSet] = new NaturalTransformationToSet[FunctorToSet] {
      override val source = t.source
      override val target = t.target
      override def apply(o: sourceCategory.O) = t(o)
    }

    lazy val adjoinInitialObject = ??? // probably never used
    lazy val adjoinTerminalObject = ???
  }
}