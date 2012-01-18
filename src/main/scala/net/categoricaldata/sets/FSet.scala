package net.categoricaldata.sets
import net.tqft.toolkit.collections.NonStrictIterable

trait FSet { fset =>
  def toIterable: Iterable[Any]
  def identity: FFunction = IdentityFunction(this)

  override def equals(other: Any): Boolean = {
    other match {
      case other: FSet => {
        if(hashCode != other.hashCode) return false
        toSet == other.toSet
      }
      case _ => false
    }
  }

  /**
   * The default implementation calls sizeIfFinite, which may be expensive.
   */

  def finite: Boolean = sizeIfFinite.nonEmpty
  def sizeIfFinite: Option[Int]
  def size: Int = sizeIfFinite.getOrElse(???)

  override def toString = toSet.toString
  override lazy val hashCode = toSet.hashCode

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
  def toSet = toIterable.toSet
  def toStringList = toList.map(_.toString)

  lazy val isEmpty = toIterable.isEmpty
}

object FSet {
  	implicit def asSet(i: Iterable[Any]): FSet = new FSet {
	  override def toIterable = i
	  override def sizeIfFinite = Some(i.size)
	}
}

trait FiniteFSet extends FSet {
  override def finite = true
  override lazy val size = toIterable.size
  override def sizeIfFinite = Some(size)
}

class ProductSet(sets: Map[Any, FSet]) extends FSet {
  def this(sets: FSet*) = this((sets.zipWithIndex map { case (x, i) => i -> x }).toMap[Any, FSet])
  override def toIterable = {
    sets.foldLeft(
      NonStrictIterable[Map[Any, Any]](Map()))(
        { case (iterable, (i, s)) => for (m <- iterable; s0 <- s.toIterable) yield m + (i -> s0) })
  }
  override lazy val sizeIfFinite = {
    val sizes = sets.values.map(_.sizeIfFinite)
    if (sizes.exists(_ == Some(0))) {
      Some(0)
    } else if (sizes.exists(_ == None)) {
      None
    } else {
      Some(sizes.map(_.get).product)
    }
  }
}
class CoproductSet(sets: Map[Any, FSet]) extends FSet {
  def this(sets: FSet*) = this((sets.zipWithIndex map { case (x, i) => i -> x }).toMap[Any, FSet])

  override def toIterable = for ((i, s) <- sets.view; x <- s.toIterable) yield (i, x)
  override lazy val sizeIfFinite = {
    val sizes = sets.values.map(_.sizeIfFinite)
    if (sizes.exists(_ == None)) {
      None
    } else {
      Some(sizes.map(_.get).sum)
    }
  }
}
