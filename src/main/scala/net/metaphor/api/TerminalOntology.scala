package net.metaphor.api

trait TerminalFinitelyGeneratedCategory[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends FinitelyPresentedCategory[O, M, C] with TerminalObject[O, M] { self: C =>
  override def objects = List(terminalObject)
  override def generators(source: O, target: O) = List(morphismFrom(terminalObject))
}
trait TerminalFinitelyPresentedCategory[O, M, C <: FinitelyPresentedCategory[O, M, C]] extends TerminalFinitelyGeneratedCategory[O, M, C] { self: C =>
  override def relations(source: O, target: O) = Nil
}

// TODO implement InitialOntology similarly
object TerminalOntology extends TerminalFinitelyPresentedCategory[Box, Path, Ontology] with Ontology {
  val terminalObject = Box("*")
  def morphismFrom(o: Box) = terminalObject.identity
}
