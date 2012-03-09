package net.categoricaldata.sets


trait FFunction { function =>
  def source: FSet
  def target: FSet
  def toFunction: Any => Any

//    def verify: this.type = {
//      for (x <- source.toIterable) require(target.toList.contains(toFunction(x)))
//      this
//    }

  def andThen[C](other: FFunction) = new FFunction {
    def source = function.source
    def target = other.target
    def toFunction = function.toFunction andThen other.toFunction
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: FFunction => {
        if (source != other.source) return false
        if (target != other.target) return false
        for (x <- source.toIterable) { if (toFunction(x) != other.toFunction(x)) return false }
        return true
      }
    }
  }
  override def hashCode = (source, target, toMap).hashCode

  override def toString = {
    toMap.toString
  }

  def toMap: Map[Any, Any] = {
    val f = toFunction
    (for (o <- source.toIterable) yield (o -> f(o))).toMap
  }

  def toStringMap: Map[String, String] =
    toMap.map({ case (a, b) => a.toString -> b.toString })

  def isomorphism_? = {
    toMap.values.toSet.size == source.size
  }
}

case class IdentityFunction(set: FSet) extends FFunction {
  override def source = set
  override def target = set
  override def toFunction = { a: Any => a }
}

object FFunction {
  def apply[A](source: FSet, target: FSet, function: A => Any) = construct(source, target, function)
  def bijection[A](source: FSet, target: FSet, function: A => Any) = construct(source, target, function, Some(true))

  private def construct[A](source: FSet, target: FSet, function: A => Any, bijection: Option[Boolean] = None): FFunction = {
    val source_ = source
    val target_ = target
    val result = new FFunction {
      override val source = source_
      override val target = target_
      override val toFunction = function.asInstanceOf[Any => Any]
      override def isomorphism_? = bijection.getOrElse(super.isomorphism_?)
    }
    result
  }
}