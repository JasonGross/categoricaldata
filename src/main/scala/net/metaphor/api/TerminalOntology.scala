package net.metaphor.api

//trait TerminalFinitelyGeneratedCategory[OO, MM, C <: FinitelyPresentedCategory[C]] extends FinitelyPresentedCategory[C] with TerminalObject[OO, MM] { self: C =>
//  type O = OO
//  type M = MM
//  val minimumLevel = 0
//  val maximumLevel = 0
//  def objectsAtLevel(k: Int) = if(k == 0) List(terminalObject) else Nil
//  override def generators(source: O, target: O) = List(morphismFrom(terminalObject))
//}
//trait TerminalFinitelyPresentedCategory[O, M, C <: FinitelyPresentedCategory[C]] extends TerminalFinitelyGeneratedCategory[O, M, C] { self: C =>
//  override def relations(source: O, target: O) = Nil
//}
//
//// TODO implement InitialOntology similarly
//object TerminalOntology extends TerminalFinitelyPresentedCategory[Box, Path, Ontology] with Ontology {
//  val terminalObject = Box("*")
//  def morphismFrom(o: Box) = identity(terminalObject)
//}
