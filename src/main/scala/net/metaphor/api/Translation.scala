package net.metaphor.api

trait Translation extends FinitelyGeneratedFunctor[Ontology] { translation =>
  val source: Ontology
  val target: Ontology

  def assertFiniteTarget: Translation with FiniteTarget = ???
}

trait FiniteTarget extends Translation { translation =>
  override val target: Ontologies.Finite
  abstract class SliceFunctor extends super.SliceFunctor {
    override type SC = SliceCategory
  }
  abstract class CosliceFunctor extends super.CosliceFunctor {
    override type cSC = CosliceCategory
  }


  lazy val slice: SliceFunctor = new SliceFunctor {
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
  lazy val coslice: CosliceFunctor = new CosliceFunctor {
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
        slice(o).functor.pullback(i).limitSet
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
        coslice(o).functor.pullback(i).colimitSet
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
