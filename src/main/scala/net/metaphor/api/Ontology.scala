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

  override def adjoinTerminalObject(b: Box) = new Ontology with WithTerminalObject {
    val terminalObject = b
    def morphismFrom(o: Box) = ???
    
    def objects = b :: ontology.objects
    def generators(source: Box, target: Box) = {
      if(target == b) {
        List(Arrow(source, target, "*").asPath)
      } else {
        ontology.generators(source, target)
      }
    }
    def relations(source: Box, target: Box) = {
      if(target == b) {
        ???
      } else {
        ontology.relations(source, target)
      }}
    def toString () = {
      println("Ontology")
      println("Objects: ")
      for (i <- objects) println(i)
      println();println("Arrows: ")
      for (i <- arrows) println(i)}
    
  }
  
  trait Dataset extends FunctorToSet with net.metaphor.api.Dataset
  trait Datamap extends NaturalTransformationToSet with net.metaphor.api.Datamap
  
  object Datasets extends FunctorsToSet
}

object Ontologies extends FinitelyPresentedCategories[Box, Path, Ontology]

trait Translation extends Functor[Box, Path, Ontology] {
  val source: Ontology
  val target: Ontology
  // FIXME, not compiling yet
//  def pullback: HeteroFunctor[source.FunctorToSet, source.NaturalTransformationToSet, source.FunctorsToSet, target.FunctorToSet, target.NaturalTransformationToSet, target.FunctorsToSet] = ???
}
