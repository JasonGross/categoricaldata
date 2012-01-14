package net.categoricaldata.api
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.permutations.Permutations

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

}

case class IdentityFunction(set: FSet) extends FFunction {
  override def source = set
  override def target = set
  override def toFunction = { a: Any => a }
}

object FFunction {
  def apply[A](source: FSet, target: FSet, function: A => Any) = {
    val source_ = source
    val target_ = target
    val result = new FFunction {
      override val source = source_
      override val target = target_
      override val toFunction = function.asInstanceOf[Any => Any]
    }
    //    result.verify
    result
  }
}

trait Sets extends Category with InitialObject with TerminalObject with Products with Coproducts {
  type O = FSet
  type M = FFunction
  override def identity(set: FSet) = set.identity
  override def source(f: FFunction) = f.source
  override def target(f: FFunction) = f.target
  override def compose(first: FFunction, second: FFunction) = first andThen second

  override val terminalObject: FSet = List("*")
  override val initialObject: FSet = Nil
  override def morphismToTerminalObject(s: FSet) = FFunction(s, terminalObject, { a: Any => "*" })
  override def morphismFromInitialObject(s: FSet) = FFunction(initialObject, s, { a: Any => throw new IllegalArgumentException })

  override def product(xs: FSet*) = new ProductSet(xs: _*)
  override def productProjections(xs: FSet*): List[FFunction] = xs.toList map {
    x: FSet =>
      {
        FFunction(product(xs), x, { m: Map[FSet, Any] => m(x) })
      }
  }
  override def productUniversality(o: FSet, ms: List[FFunction]) = {
    val xs = ms.map(_.target)
    FFunction(o, product(xs: _*), { e: Any => (for (m <- ms) yield m.target -> m.toFunction(o)).toMap })
  }

  override def coproduct(xs: FSet*) = new CoproductSet(xs: _*)
  override def coproductInjections(xs: FSet*): List[FFunction] = xs.toList map {
    x: FSet =>
      {
        FFunction(x, coproduct(xs), { e: Any => (x, e) })
      }
  }
  override def coproductUniversality(o: FSet, ms: List[FFunction]) = {
    val xs = ms.map(_.source)
    FFunction(coproduct(xs), o, { p: (Int, Any) => ms(p._1).toFunction(p._2) })
  }

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
      case _ => ???
    }
  }

  def functions(set1: FSet, set2: FSet): FSet = {
    val maps = set1.toIterable.foldLeft(
      NonStrictIterable[Map[Any, Any]](Map()))(
        {
          case (iterable, a) => for (m <- iterable; s0 <- set2.toIterable) yield {
            ???
            m + (a -> s0)
          }
        })
    maps.map(m => FFunction(set1, set2, m))
  }
}

object Sets extends Sets {
  override def toString = "Sets"
}
