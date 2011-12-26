package net.metaphor.api

//object InitialObject {
//  implicit def InitialObjectToObject[O, M](i: InitialObject[O, M]) = i.initialObject
//}
object TerminalObject {
  implicit def TerminalObjectToObject[O, M](i: TerminalObject[O, M]) = i.terminalObject
}

trait InitialObject[O, M] {
	def initialObject: O
	def morphismTo(o: O): M
}
trait TerminalObject[O, M] {
	def terminalObject: O
	def morphismFrom(o: O): M
}


// TODO one day, TerminalObject should look like this:
//trait TerminalObject[C <: Category[C]] { category: Category[C] => 
//  def terminalObject: category.O
//  def morphismFrom(o: category.O): category.M
//}