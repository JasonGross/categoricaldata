package net.metaphor.api

trait Set
trait Function extends Morphism

object Sets extends Category[Set, Function] {
	def identity(set: Set) = ???
	def compose(first: Function, second: Function) = ???
}
