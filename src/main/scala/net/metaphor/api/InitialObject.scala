package net.metaphor.api

trait InitialObject[O, M] {
	def initialObject: O
	def morphismTo(o: O): M
}
trait FinalObject[O, M] {
	def finalObject: O
	def morphismFrom(o: O): M
}

trait Diagram[O, M, C <: Category[O, M]] 

trait CoCone[O, M, C <: Category[O, M]] {
  def source: Diagram[O, M, C]
  def target: Diagram[O, M ,C] with FinalObject[O, M]
}

trait CoConeMap {
  
}

trait Colimit2[O, M, C <: Category[O, M]] extends InitialObject[CoCone[O, M, C], CoConeMap]