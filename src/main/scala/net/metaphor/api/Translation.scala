package net.metaphor.api

trait Translation extends Functor[Box, Path, Ontology] { translation =>
  val source: Ontology
  val target: Ontology

  trait ContravariantDataFunctor extends super.ContravariantDataFunctor {
    // TODO pull up? this trait could vanish.
    // the following two 'apply' methods are convenience methods, allowing us to act on a Dataset or Datamap which is not an inner type.
    def apply(i: Ontology#Dataset) = {
      super.apply(translation.target.Dataset(i))
    }
    def apply(m: Ontology#Datamap) = {
      super.apply(translation.target.Datamap(m))
    }
  }
  trait CovariantDataFunctor extends HeteroFunctor[source.Dataset, source.Datamap, source.Datasets, target.Dataset, target.Datamap, target.Datasets] {
    val source = translation.source.Datasets
    val target = translation.target.Datasets

    // the following two 'apply' methods are convenience methods, allowing us to act on a Dataset or Datamap which is not an inner type.
    def apply(i: Ontology#Dataset) = {
      super.apply(translation.source.Dataset(i))
    }
    def apply(m: Ontology#Datamap) = {
      super.apply(translation.source.Datamap(m))
    }
  }

  trait Pullback extends ContravariantDataFunctor

  def pullback: Pullback = new Pullback {
    def onObjects(i: translation.target.Dataset) = new translation.source.Dataset {
      def onObjects(o: Box) = i(translation(o))
      def onMorphisms(m: Path) = i(translation(m))
    }
    def onMorphisms(m: translation.target.Datamap) = new translation.source.Datamap {
      def source = onObjects(m.source)
      def target = onObjects(m.target)
      def apply(o: Box) = m(translation(o))
    }
  }

  def ^* = pullback

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

  trait Pushforward extends CovariantDataFunctor
  trait Shriek extends CovariantDataFunctor

  def pushforward: Pushforward = new Pushforward {
    def onObjects(i: translation.source.Dataset) = new translation.target.Dataset {
      def onObjects(o: Box) = {
        // here's the rough idea: 
//            		  slice(o).functor.pullback(i).limitSet
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
  def shriek: Shriek = new Shriek {
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

  def __! = shriek
  def __* = pushforward
}
