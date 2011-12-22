package net.metaphor.api

trait Translation extends Functor[Box, Path, Ontology] { translation =>
  val source: Ontology
  val target: Ontology

  trait ContravariantDataFunctor extends HeteroFunctor[target.Dataset, target.Datamap, target.Datasets, source.Dataset, source.Datamap, source.Datasets] {
    val source = translation.target.Datasets
    val target = translation.source.Datasets
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
    def apply(i: Ontology#Dataset) = {
      super.apply(translation.source.Dataset(i))
    }
    def apply(m: Ontology#Datamap) = {
      super.apply(translation.source.Datamap(m))
    }
  }

  trait Pullback extends ContravariantDataFunctor
  trait Pushforward extends CovariantDataFunctor
  trait Shriek extends CovariantDataFunctor

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
  def pushforward: Pushforward = ???
  def shriek: Shriek = ???

  def ^* = pullback
  def __! = shriek
  def __* = pushforward
}
