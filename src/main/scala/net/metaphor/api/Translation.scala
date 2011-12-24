package net.metaphor.api

trait Translation extends FinitelyGeneratedFunctor[Ontology] { translation =>
  val source: Ontology
  val target: Ontology

  def assertFiniteTarget: Translation with FiniteTarget = ???
}

trait FiniteTarget extends Translation { translation =>
  override val target: Ontologies.Finite
  abstract class CommaFunctor extends super.CommaFunctor {
    override val target = translation.target.categoriesOver
    
    override type SC =  SliceCategory
  }

//    class CosliceCategory(onLeft: translation.target.O) extends FinitelyGeneratedCategory[CosliceCategory] {
//      override type O = ObjectRightOf
//      override type M = ObjectRightOfMap
//
//      case class ObjectLeftOf(right: translation.source.O, path: translation.target.M) {
//        require(path.target == right)
//        require(path.target == translation(onRight))
//      }
//      case class ObjectLeftOfMap(source: ObjectLeftOf, target: ObjectLeftOf, path: translation.source.M) {
//        require(path.source == source.left)
//        require(path.target == target.left)
//        require(translation.target.compose(translation(path), target.path) == source.path)
//      }
//
//      def objectsAtLevel(k: Int): List[ObjectLeftOf] = {
//        for (
//          l <- (0 to k).toList;
//          left <- translation.source.objectsAtLevel(l);
//          path <- translation.target.wordsOfLength(k - l)(left, translation(onRight))
//        ) yield ObjectLeftOf(left, path)
//      }
//      val minimumLevel: Int = 0
//      val maximumLevel: Int = translation.source.maximumLevel + translation.target.maximumWordLength
//
//      def generators(source: ObjectLeftOf, target: ObjectLeftOf): List[ObjectLeftOfMap] = {
//        for(g <- translation.source.generators(source.left, target.left); if translation.target.compose(translation(g), target.path) == source.path) yield {
//          ObjectLeftOfMap(source, target, g)
//        }
//      }
//
//      def identity(o: ObjectLeftOf) = ObjectLeftOfMap(o, o, translation.target.identity(o.left))
//      def source(m: ObjectLeftOfMap) = m.source
//      def target(m: ObjectLeftOfMap) = m.target
//      def compose(m1: ObjectLeftOfMap, m2: ObjectLeftOfMap) = ObjectLeftOfMap(m1.source, m2.target, translation.source.compose(m1.path, m2.path))
//
//      def opposite = ??? // new SliceCategory(onRight) with Opposite
//
//      val functorsToSet = ???
//      val adjoinInitialObject = ???
//      val adjoinTerminalObject = ???
//      type CSets = FunctorsToSet
//
//      override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[SliceCategory]): F = ???
//      override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[SliceCategory, F]): T = ???
//    }
//  }

  lazy val slice: CommaFunctor = new CommaFunctor {
    class SliceCategoryOver(s: Box) extends translation.target.CO { sliceCategory =>
      val functor: translation.target.FunctorTo[Ontology] = new translation.target.FunctorTo[Ontology] {
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

    override val source = translation.target
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
      def onObjects(o: Box) = {
        //slice(o).functor.pullback(i).limitSet
        ???
      }
      def onMorphisms(m: Path) = ???
    }
    def onMorphisms(m: translation.source.Datamap) = new translation.target.Datamap {
      val source = onObjects(m.source)
      val target = onObjects(m.target)
      def apply(o: Box) = ???
    }
  }
  trait Shriek extends CovariantDataFunctor {
    def onObjects(i: translation.source.Dataset) = new translation.target.Dataset {
      def onObjects(o: Box) = {
        //      coslice(o).functor.pullback(i).colimitSet
        ???
      }
      def onMorphisms(m: Path) = ???
    }
    def onMorphisms(m: translation.source.Datamap) = new translation.target.Datamap {
      val source = onObjects(m.source)
      val target = onObjects(m.target)
      def apply(o: Box) = ???
    }
  }

  def pushforward: Pushforward = new Pushforward {}
  def shriek: Shriek = new Shriek {}
  def __! = shriek
  def __* = pushforward
}
