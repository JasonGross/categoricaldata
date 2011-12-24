package net.metaphor.api

trait Translation extends SmallFunctor[Box, Path, Ontology] { translation =>
  val source: Ontology
  val target: Ontology

  def assertFiniteTarget: Translation with FiniteTarget = ???
}

trait FiniteTarget extends Translation { translation =>
  override val target: Ontologies.Finite

  trait CommaFunctor extends HeteroFunctor[Box, Path, Ontology, target.CO, target.FO, target.CsO] {
    override def target = translation.target.categoriesOver

    case class ObjectLeftOf(left: Box, path: Path, of: Box) {
      require(path.source == left)
      require(path.target == translation(of))
    }
    case class ObjectLeftOfMap(source: ObjectLeftOf, target: ObjectLeftOf, path: Path) {
      require(path.source == source.left)
      require(path.target == target.left)
    }

    class SliceCategory(s: Box) extends FinitelyGeneratedCategory[ObjectLeftOf, ObjectLeftOfMap, SliceCategory] {
      def objectsAtLevel(k: Int): List[ObjectLeftOf] = ???
      val minimumLevel: Int = ???
      val maximumLevel: Int = ???

      def generators(source: ObjectLeftOf, target: ObjectLeftOf): List[ObjectLeftOfMap] = ???

      def identity(o: ObjectLeftOf) = ObjectLeftOfMap(o, o, translation.target.identity(o.left))
      def source(m: ObjectLeftOfMap) = m.source
      def target(m: ObjectLeftOfMap) = m.target
      def compose(m1: ObjectLeftOfMap, m2: ObjectLeftOfMap) = ObjectLeftOfMap(m1.source, m2.target, translation.source.compose(m1.path, m2.path))

      def opposite = new SliceCategory(s) with Opposite

      val functorsToSet = ???
      val adjoinInitialObject = ???
      val adjoinTerminalObject = ???
      type CSets = FunctorsToSet

      override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[ObjectLeftOf, ObjectLeftOfMap, SliceCategory]): F = ???
      override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[ObjectLeftOf, ObjectLeftOfMap, SliceCategory, F]): T = ???

    }
  }

  lazy val slice: CommaFunctor = new CommaFunctor {
    class SliceCategoryOver(s: Box) extends translation.target.CO { sliceCategory =>
      val functor: translation.target.FunctorTo[Box, Path, Ontology] = new translation.target.FunctorTo[Box, Path, Ontology] {
        val source = sliceCategory.category
        def onObjects(o: Box) = ???
        def onMorphisms(m: Path) = ???
      }
      val category: Ontology = {
        //        new SliceCategory(s).asInstanceOf[Ontology] // FIXME that cast is nonsense
        ???
      }
    }
    class SliceFunctorOver(m: Path) extends translation.target.FO {
      def source = new SliceCategoryOver(translation.target.source(m))
      def target = new SliceCategoryOver(translation.target.target(m))
      def functor = ???
    }

    override def source = translation.target
    override def onObjects(s: Box): translation.target.CO = new SliceCategoryOver(s)
    override def onMorphisms(m: Path): translation.target.FO = new SliceFunctorOver(m)
  }
  lazy val coslice: CommaFunctor = new CommaFunctor {
    class CosliceCategoryOver(s: Box) extends translation.target.CO { cosliceCategory =>
      val functor = ???
      val category = ???
    }
    class CosliceFunctorOver(m: Path) extends translation.target.FO {
      def source = new CosliceCategoryOver(translation.target.source(m))
      def target = new CosliceCategoryOver(translation.target.target(m))
      def functor = ???
    }

    override val source = translation.target.opposite
    override def onObjects(s: Box): translation.target.CO = new CosliceCategoryOver(s)
    override def onMorphisms(m: Path): translation.target.FO = new CosliceFunctorOver(m)
  }

  trait Pushforward extends CovariantDataFunctor {
    def onObjects(i: translation.source.Dataset) = new translation.target.Dataset {
      def onObjects(o: Box) = slice(o).functor.pullback(i).limitSet
      def onMorphisms(m: Path) = ???
    }
    def onMorphisms(m: translation.source.Datamap) = new translation.target.Datamap {
      def source = onObjects(m.source)
      def target = onObjects(m.target)
      def apply(o: Box) = ???
    }
  }
  trait Shriek extends CovariantDataFunctor {
    def onObjects(i: translation.source.Dataset) = new translation.target.Dataset {
      def onObjects(o: Box) = coslice(o).functor.pullback(i).colimitSet
      def onMorphisms(m: Path) = ???
    }
    def onMorphisms(m: translation.source.Datamap) = new translation.target.Datamap {
      def source = onObjects(m.source)
      def target = onObjects(m.target)
      def apply(o: Box) = ???
    }
  }

  def pushforward: Pushforward = new Pushforward {}
  def shriek: Shriek = new Shriek {}
  def __! = shriek
  def __* = pushforward
}
