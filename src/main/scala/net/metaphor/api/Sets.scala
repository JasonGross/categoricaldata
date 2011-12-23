package net.metaphor.api

trait Set {
  def toIterable: Iterable[Any]
  def identity: Function = IdentityFunction(this)
  override def equals(other: Any) = {
    other match {
      case other: Set => {
        toIterable == other.toIterable
      }
      case _ => false
    }
  }
}
trait Function { function =>
  def toFunction: Any => Any
  def andThen(other: Function) = new Function {
    def toFunction = function.toFunction andThen other.toFunction
  }
}
case class IdentityFunction[A](set: Set) extends Function {
  override def toFunction = { a: Any => a }
}

trait Sets extends Category[Set, Function, Sets]

object Sets extends Sets {
  override def identity(set: Set) = set.identity
  override def source(f: Function) = ???
  override def target(f: Function) = ???
  override def compose(first: Function, second: Function) = first andThen second

  type F = FunctorToSet
  type T = NaturalTransformationToSet[F]
  type CSets = FunctorsToSet
}
