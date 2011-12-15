package net.metaphor.api

trait InitialObject[O, M] {
	def initialObject: O
	def morphismTo(o: O): M
}
trait TerminalObject[O, M] {
	def terminalObject: O
	def morphismFrom(o: O): M
}

