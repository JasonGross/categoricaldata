package net.metaphor.api

trait FinitelyGeneratedCategory[O, M, C <: FinitelyGeneratedCategory[O, M, C]] extends Category[O, M, C] { self: C =>
  def objects: List[O]
  def generators(source: O, target: O): List[M]
  def generators: List[M] = for (s <- objects; t <- objects; g <- generators(s, t)) yield g
}

trait FinitelyGeneratedCategories[O, M, C <: FinitelyGeneratedCategory[O, M, C]] extends Categories[O, M, C]

trait FinitelyPresentedCategory[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FinitelyGeneratedCategory[O, M, C] { self: C =>
  def relations(source: O, target: O): List[M]
  def relations: List[M] = for (s <- objects; t <- objects; r <- relations(s, t)) yield r
    
  def adjoinTerminalObject(o: O): C with TerminalObject[O, M] = ???
    
  class FunctorsToSet extends net.metaphor.api.FunctorsToSet[O, M ,C](self) {
    
  }
}

trait FinitelyPresentedCategories[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FinitelyGeneratedCategories[O, M, C]

