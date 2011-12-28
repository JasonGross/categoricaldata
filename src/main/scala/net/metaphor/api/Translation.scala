package net.metaphor.api

trait Translation extends FinitelyGeneratedFunctor { translation =>
  override val source: Ontology
  override val target: Ontology

  override type SC = SliceCategory
  override type cSC = CosliceCategory

  def assertFiniteTarget: Translation with FiniteTarget = ???
}

trait FiniteTarget extends Translation { translation =>
  override val target: Ontologies.Finite

  class SliceFunctor extends super.SliceFunctor {
    override def buildSliceCategory(onRight: Box) = new SliceCategory(onRight)
  }
  class CosliceFunctor extends super.CosliceFunctor {
    override def buildCosliceCategory(onLeft: Box) = new CosliceCategory(onLeft)
  }

  lazy val slice: SliceFunctor = new SliceFunctor
  lazy val coslice: CosliceFunctor = new CosliceFunctor

  trait Pushforward extends CovariantDataFunctor { pushforward =>
    override def onObjects(i: translation.source.Dataset) = new translation.target.Dataset {
      def onObjects(o: Box) = {
        ???
        //        slice(o).functor.pullback(i).limitSet
      }
      def onGenerators(g: translation.target.G) = ???
    }
    override def onMorphisms(m: translation.source.Datamap) = new translation.target.Datamap {
      val source = pushforward.onObjects(m.source)
      val target = pushforward.onObjects(m.target)
      def apply(o: Box) = ???
    }
  }
  trait Shriek extends CovariantDataFunctor { shriek =>
    override def onObjects(i: translation.source.Dataset) = new translation.target.Dataset {
      def onObjects(o: Box) = {
        val z = coslice
        val a = coslice(o)
        val b = a.functor
        val c = b.pullback
        val d: c.target.O = c(i)
        val CCC = b.source
//        val e = d.colimitSet // FIXME
//        coslice(o).functor.pullback(i).colimitSet
        ???
      }
      def onGenerators(m: translation.target.G) = ???
    }
    override def onMorphisms(m: translation.source.Datamap) = new translation.target.Datamap {
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
