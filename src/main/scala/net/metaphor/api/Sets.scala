package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.permutations.Permutations

trait FSet { fset =>
  def toIterable: Iterable[Any]
  def identity: FFunction = IdentityFunction(this)

  override def equals(other: Any) = {
    other match {
      case other: FSet => {
        hashCode == other.hashCode &&
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
  override lazy val hashCode = toIterable.toSet[Any].hashCode

  private class CachingFSet extends FSet {
    def sizeIfFinite = fset.sizeIfFinite
    val toIterable = {
      import net.tqft.toolkit.collections.CachingIterable
      CachingIterable(fset.toIterable)
    }
  }
  
  private class ForcedFSet extends FiniteFSet {
    val toIterable = fset.toIterable.toList
  }
  
  def cache: FSet = new CachingFSet
  def force: FSet = new ForcedFSet
  
  def toList = toIterable.toList
  def toStringList = toList.map(_.toString)
}

trait FiniteFSet extends FSet {
  override def finite = true
  override lazy val size = toIterable.size
  override def sizeIfFinite = Some(size)
}

trait FFunction { function =>
  def source: FSet
  def target: FSet
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

  override def toString = {
    toMap.toString
  }
  
  def toMap: Map[Any, Any] = {
    val f = toFunction
    (for (o <- source.toIterable) yield (o -> f(o))).toMap
  }
  
  def toStringMap: Map[String, String] = 
    toMap.map({ case (a,b) => a.toString -> b.toString })
}
case class IdentityFunction(set: FSet) extends FFunction {
  override def source = set
  override def target = set
  override def toFunction = { a: Any => a }
}

object FFunction {
  def apply(source: FSet, target: FSet, function: Any => Any) = {
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
  type O = FSet
  type M = FFunction
  override def identity(set: FSet) = set.identity
  override def source(f: FFunction) = f.source
  override def target(f: FFunction) = f.target
  override def compose(first: FFunction, second: FFunction) = first andThen second

  def bijections(set1: FSet, set2: FSet): FSet = {
    (set1.sizeIfFinite, set2.sizeIfFinite) match {
      case (Some(k), Some(l)) if k == l => {
        val set2List = set2.toIterable.toList
        for (p <- Permutations.of(k)) yield {
          val m = (for ((x, i) <- set1.toIterable zip p) yield {
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

