package net.metaphor.api

trait FinitelyGeneratedCategory[O, M] extends Category[O, M] {
  def objects: List[O]
  def generators(source: O, target: O): List[M]
  def generators: List[M]
}

trait FinitelyPresentedCategory[O, M] extends FinitelyGeneratedCategory[O, M] {
  def relations(source: O, target: O): List[M]
  def relations: List[M]
}

trait FinitelyPresentedCategories[O, M] extends Categories[O, M, FinitelyPresentedCategory[O, M]]

