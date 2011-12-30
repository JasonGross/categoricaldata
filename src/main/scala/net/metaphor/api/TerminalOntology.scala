package net.metaphor.api

//trait TerminalFinitelyGeneratedCategory extends FinitelyPresentedCategory with TerminalObject { 
//  val minimumLevel = 0
//  val maximumLevel = 0
//  def objectsAtLevel(k: Int) = if(k == 0) List(terminalObject) else Nil
//  override def generators(source: O, target: O) = Nil
//}
//trait TerminalFinitelyPresentedCategory extends TerminalFinitelyGeneratedCategory { 
//  override def relations(source: O, target: O) = Nil
//}
//
//// TODO implement InitialOntology similarly
//object TerminalOntology extends TerminalFinitelyPresentedCategory with Ontology {
//  val terminalObject = Box("*")
//  def morphismFrom(o: Box) = identity(terminalObject)
//}
