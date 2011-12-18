package net.metaphor.api

case class Box(name: String) {
  def identity = Path(this, Nil)
}

case class Arrow(source: Box, target: Box, name: String) {
  def asPath = Path(source, List(this))
}

case class Path(start: Box, arrows: List[Arrow]) {
  def finish = arrows.last.target
}

trait Dataset extends FunctorToSet[Box, Path, Ontology]
trait Datamap extends NaturalTransformationToSet[Box, Path, Ontology]

trait Ontology extends FinitelyPresentedCategory[Box, Path, Ontology] { ontology =>
  override def compose(m1: Path, m2: Path) = {
    require(m2.finish == m1.start)
    Path(m1.start, m1.arrows ::: m2.arrows)
  }
  override def source(m: Path) = m.start
  override def target(m: Path) = m.finish
  override def identity(o: Box) = o.identity

  trait Dataset extends FunctorToSet with net.metaphor.api.Dataset
  trait Datamap extends NaturalTransformationToSet with net.metaphor.api.Datamap
  
  class Datasets extends FunctorsToSet[Box, Path, Ontology](ontology) {
    // how far up can we lift this?
    def colimit(functor: Dataset) = {
      new InitialObject[Dataset, Datamap] {
        def initialObject = ???
        def morphismTo(other: Dataset) = {
          // require that the source is (source + terminal object)?
          // require that it actually extends functor?
          ???
        }
      }
    }
  }
}

object Ontologies extends FinitelyPresentedCategories[Box, Path, Ontology] {
  // hmm, we probably need this to make up 'o' and 'f' internally
  override def adjoinTerminalObject(category: Ontology, o: Box, f: Box => Path) = {
    new Ontology with TerminalObject[Box, Path] {
      def morphismFrom(b: Box) = f(b)
      val terminalObject = o
      val objects = o :: category.objects
      def generators(source: Box, target: Box) = {
        if (target == o) {
          List(f(source))
        } else {
          category.generators(source, target)
        }
      }
      def relations(source: Box, target: Box) = {
        if (target == o) {
          ??? // FIXME
        } else {
          category.relations(source, target)
        }
      }
    }
  }

  // TODO adjoinInitialObject
}

trait Translation extends Functor[Box, Path, Ontology]
