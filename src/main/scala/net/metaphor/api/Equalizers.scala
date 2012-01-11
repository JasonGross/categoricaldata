package net.metaphor.api

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

// TODO Coequalizers
