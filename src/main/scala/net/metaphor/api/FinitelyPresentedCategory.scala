package net.metaphor.api

trait FinitelyGeneratedCategory[O, M] extends Category[O, M] {
  def objects: List[O]
  def generators(source: O, target: O): List[M]
  def generators: List[M] = for (s <- objects; t <- objects; g <- generators(s, t)) yield g
}

trait FinitelyPresentedCategory[O, M] extends FinitelyGeneratedCategory[O, M] { self =>
  def relations(source: O, target: O): List[M]
  def relations: List[M] = for (s <- objects; t <- objects; r <- relations(s, t)) yield r
}

trait FinitelyPresentedCategories[O, M, C <: FinitelyPresentedCategory[O, M]] extends Categories[O, M, C]

