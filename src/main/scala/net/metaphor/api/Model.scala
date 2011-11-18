package net.metaphor.api

trait Box {
  def name: String
  def description: String
}

trait Arrow {
  def source: Box
  def target: Box
  def name: String
  def description: String
}

trait Path {
  def start: Box
  def end: Box
  def arrows: List[Arrow]
}

trait Ontology extends FinitelyGeneratedCategory[Box, Path] {

}

object Ontologies extends FinitelyPresentedCategories[Box, Path]

trait Translation extends Functor[Box, Path]

trait Model {
  val over: Ontology
}
