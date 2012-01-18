package net.categoricaldata.sets

import net.categoricaldata.universalalgebra._
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.permutations.Permutations

trait Sets extends net.categoricaldata.category.Category with InitialObject with TerminalObject with Products with Coproducts {
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
          FFunction(set1, set2, m) // TODO declare these to be isomorphisms?
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

