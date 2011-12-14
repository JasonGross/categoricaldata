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

trait Ontology extends FinitelyPresentedCategory[Box, Path] {
  override def compose(m1: Path, m2: Path) = {
    require(m2.finish == m1.start)
    Path(m1.start, m1.arrows ::: m2.arrows)
  }
  override def source(m: Path) = m.start
  override def target(m: Path) = m.finish
  override def identity(o: Box) = o.identity
}

object Ontologies extends FinitelyPresentedCategories[Box, Path, Ontology]

trait Translation extends Functor[Box, Path, Ontology]

trait Dataset extends FunctorToSet[Box, Path, Ontology]

trait Model {
  val over: Ontology
}
