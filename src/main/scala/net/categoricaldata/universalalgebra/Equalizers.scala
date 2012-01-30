package net.categoricaldata.universalalgebra
import net.categoricaldata.category._

trait Equalizers { category: Category =>
  class Cones(ms: category.M*) extends Category {
    override type O = category.M
    override type M = (category.M, category.M, category.M)

    override def identity(o: O) = (o, o, category.identity(category.source(o)))
    override def source(m: M) = m._1
    override def target(m: M) = m._2
    override def compose(m1: M, m2: M) = (m1._1, m2._3, category.compose(m1._3, m2._3))

  }

  def equalizerInitialCone(ms: category.M*): Cones with TerminalObject = new Cones(ms: _*) with TerminalObject {
    override def terminalObject = equalizerMorphism(ms: _*)
    override def morphismToTerminalObject(m: category.M) = (m, terminalObject, equalizerUniversality(m, ms: _*))
  }
  def equalizerMorphism(ms: category.M*): M
  def equalizerObject(ms: category.M*): O = category.source(equalizerMorphism(ms: _*))
  def equalizerUniversality(m: category.M, ms: category.M*): M
}

trait Coequalizers { category: Category =>
  class Cocones(ms: category.M*) extends Category {
    override type O = category.M
    override type M = (category.M, category.M, category.M)

    override def identity(o: O) = (o, o, category.identity(category.source(o)))
    override def source(m: M) = m._1
    override def target(m: M) = m._2
    override def compose(m1: M, m2: M) = (m1._1, m2._3, category.compose(m1._3, m2._3))

  }

  def coequalizerInitialCocone(ms: category.M*): Cocones with InitialObject = new Cocones(ms: _*) with InitialObject {
    override def initialObject = coequalizerMorphism(ms: _*)
    override def morphismFromInitialObject(m: category.M) = (initialObject, m, coequalizerUniversality(m, ms: _*))
  }
  def coequalizerMorphism(ms: category.M*): M
  def coequalizerObject(ms: category.M*): O = category.target(coequalizerMorphism(ms: _*))
  def coequalizerUniversality(m: category.M, ms: category.M*): M
}
