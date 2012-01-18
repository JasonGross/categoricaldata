package net.categoricaldata.universalalgebra
import net.categoricaldata.category._

trait Products { category: Category =>
  class MorphismsToSet(os: Seq[O]) extends Category {
    override type O = (category.O, List[category.M])
    override type M = (category.M, O, O)

    override def identity(o: O) = (category.identity(o._1), o, o)
    override def source(m: M) = m._2
    override def target(m: M) = m._3
    override def compose(m1: M, m2: M) = (category.compose(m1._1, m2._1), m1._2, m2._3)
  }

  def productTerminalObject(xs: O*): MorphismsToSet with TerminalObject = new MorphismsToSet(xs) with TerminalObject { mts =>
    override def terminalObject = (product(xs: _*), productProjections(xs: _*))
    override def morphismToTerminalObject(o: mts.O) = (productUniversality(o._1, o._2), o, terminalObject)
  }

  def product(xs: O*): O
  def product(ms: M*)(implicit d: DummyImplicit): M = {
    // We want to build a map from product(ms.map(source(_))) to product(ms.map(target(_)))
    val ss = ms.map(source(_))
    productUniversality(product(ss: _*), (productProjections(ss: _*) zip ms).map({ case (p, m) => category.compose(p, m) }))
  }
  def productProjections(xs: O*): List[M]
  // given an object o and maps ms from some objects xs, the map from o to product(xs)
  def productUniversality(o: O, ms: List[M]): M
}

trait Coproducts { category: Category =>
  class MorphismsFromSet(os: Seq[O]) extends Category {
    override type O = (category.O, List[category.M])
    override type M = (category.M, O, O)

    override def identity(o: O) = (category.identity(o._1), o, o)
    override def source(m: M) = m._2
    override def target(m: M) = m._3
    override def compose(m1: M, m2: M) = (category.compose(m1._1, m2._1), m1._2, m2._3)
  }

  def productInitialObject(xs: O*): MorphismsFromSet with InitialObject = new MorphismsFromSet(xs) with InitialObject { mfs =>
    override def initialObject = (coproduct(xs: _*), coproductInjections(xs: _*))
    override def morphismFromInitialObject(o: mfs.O) = (coproductUniversality(o._1, o._2), o, initialObject)
  }

  def coproduct(xs: O*): O
  def coproduct(ms: M*)(implicit d: DummyImplicit): M = {
    // We want to build a map from product(ms.map(source(_))) to product(ms.map(target(_)))
    val ss = ms.map(source(_))
    coproductUniversality(coproduct(ss: _*), (ms.toList zip coproductInjections(ss: _*)).map({ case (m, i) => category.compose(m, i) }))
  }

  def coproductInjections(xs: O*): List[M]
  // given an object o mapping via ms to some objects xs, the map from coproductObject(x) to o
  def coproductUniversality(o: O, ms: List[M]): M

}