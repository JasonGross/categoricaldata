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

  trait WithTerminalObject extends FinitelyPresentedCategory[O, M, C] with TerminalObject[O, M] { self: C => }
  def adjoinTerminalObject(o: O): WithTerminalObject = ???

  trait FunctorToSet extends super.FunctorToSet {
    class TerminalExtensions(val terminal: WithTerminalObject) {
      trait TerminalExtension extends terminal.FunctorToSet
    }
  }

  case class TerminalExtensionsOfFunctorToSet()
  
  class FunctorsToSet extends super.FunctorsToSet {
    // how far up can we lift this?
    def colimit(functor: self.FunctorToSet)(extensions: functor.TerminalExtensions) = {
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

trait FinitelyPresentedCategories[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FinitelyGeneratedCategories[O, M, C]

