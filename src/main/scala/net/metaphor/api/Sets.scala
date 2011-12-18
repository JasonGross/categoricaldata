package net.metaphor.api

object Set {
  def apply(t: Iterable[String]) = new Set {
    def toIterable = t
  }
}
object Function {
  def apply(f: String =>  String) = new Function {
    def toFunction = f
  }
}

trait Set {
  def toIterable: Iterable[String]
  def identity: Function = IdentityFunction(this)
}
trait Function { function =>
  def toFunction: String => String
  def andThen(other: Function) = new Function {
    def toFunction = function.toFunction andThen other.toFunction
  }
}
case class IdentityFunction[A](set: Set) extends Function {
  override def toFunction = { a: String => a }
}

trait Sets extends Category[Set, Function, Sets]

object Sets extends Sets {
  override def identity(set: Set) = ???
  override def source(f: Function) = ???
  override def target(f: Function) = ???
  override def compose(first: Function, second: Function) = ???
}
