package net.metaphor.api

trait Translation extends FinitelyGeneratedFunctor[Ontology] { translation =>
  val source: Ontology
  val target: Ontology

  override type SC = SliceCategory
  override type cSC = CosliceCategory

  def assertFiniteTarget: Translation with FiniteTarget = ???
}

trait FiniteTarget extends Translation { translation =>
  override val target: Ontologies.Finite

  class SliceFunctor extends super.SliceFunctor {
    override def buildSliceCategory(onRight: Box) = new SliceCategory(onRight)
  }
  //  abstract class CosliceFunctor extends super.CosliceFunctor {
  //    override type cSC = CosliceCategory
  //    override def buildCosliceCategory(onLeft: Box) = new CosliceCategory(onLeft) { }
  //  }

  lazy val slice: SliceFunctor = new SliceFunctor
  //  lazy val coslice: CosliceFunctor = new CosliceFunctor

  trait Pushforward extends CovariantDataFunctor {
    def onObjects(i: translation.source.Dataset) = new translation.target.Dataset {
      def onObjects(o: Box) = {
        val foo = slice(o).functor.pullback(i)
        //        slice(o).functor.pullback(i).limitSet
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
        //        coslice(o).functor.pullback(i).colimitSet
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
