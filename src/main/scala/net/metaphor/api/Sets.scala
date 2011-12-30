package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.permutations.Permutations

trait Set {
  def toIterable: Iterable[Any]
  def identity: FFunction = IdentityFunction(this)

  override def equals(other: Any) = {
    other match {
      case other: Set => {
        toIterable == other.toIterable
      }
      case _ => false
    }
  }

  /**
   * The default implementation calls sizeIfFinite, which may be expensive.
   */

  def finite: Boolean = sizeIfFinite.nonEmpty
  def sizeIfFinite: Option[Int]
  def size: Int = sizeIfFinite.getOrElse(???) //???

  override def toString = toIterable.toSet[Any].toString
  override def hashCode = toIterable.toSet[Any].hashCode
}
trait FFunction { function =>
  def source: Set
  def target: Set
  def toFunction: Any => Any
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
}
case class IdentityFunction(set: Set) extends FFunction {
  override def source = set
  override def target = set
  override def toFunction = { a: Any => a }
}

object FFunction {
  def apply(source: Set, target: Set, function: Any => Any) = {
    val source_ = source
    val target_ = target
    new FFunction {
      override val source = source_
      override val target = target_
      override val toFunction = function
    }
  }
}

trait Sets extends Category {
  type O = Set
  type M = FFunction
  override def identity(set: Set) = set.identity
  override def source(f: FFunction) = f.source
  override def target(f: FFunction) = f.target
  override def compose(first: FFunction, second: FFunction) = first andThen second

  def bijections(set1: Set, set2: Set): Set = {
    (set1.sizeIfFinite, set2.sizeIfFinite) match {
      case (Some(k), Some(l)) if k == l => {
        val set2List = set2.toIterable.toList
        for (p <- Permutations.of(k)) yield {
          val m = (for ((x, i) <- set2.toIterable zip p) yield {
            x -> set2List(i)
          }).toMap
          FFunction(set1, set2, m)
        }
      }
      case _ => NonStrictIterable()
    }
  }
}

object Sets extends Sets {
  override def toString = "Sets"
}

