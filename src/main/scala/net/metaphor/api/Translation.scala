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
  }

  lazy val slice = new CommaFunctor {
    class SliceCategoryOver(s: Box) extends translation.target.CO {
      val functor: translation.target.FunctorTo[Box, Path, Ontology] = ???
      val category: Ontology = ???
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
  lazy val coslice = new CommaFunctor {
    override val source = translation.target.opposite
    override def onObjects(s: Box) = ???
    override def onMorphisms(m: Path) = ???
  }

  trait Pushforward extends CovariantDataFunctor {
    def onObjects(i: translation.source.Dataset) = new translation.target.Dataset {
      def onObjects(o: Box) = {
        // here's the rough idea: 
//         slice(o).functor.pullback(i).limitSet
        ???
      }
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
      def onObjects(o: Box) = ???
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
