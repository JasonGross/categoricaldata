package net.metaphor.api

trait Translation extends Functor[Box, Path, Ontology] { translation =>
  val source: Ontology
  val target: Ontology

  trait ContravariantDataFunctor extends HeteroFunctor[target.Dataset, target.Datamap, target.Datasets, source.Dataset, source.Datamap, source.Datasets] {
    val source = translation.target.Datasets
    val target = translation.source.Datasets
    
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

trait FiniteTarget { translation: Translation => 
  override val target: Ontology with FiniteMorphisms[Box, Path, Ontology]
  
  trait Pushforward extends CovariantDataFunctor
  trait Shriek extends CovariantDataFunctor

  def pushforward: Pushforward = new Pushforward {
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
