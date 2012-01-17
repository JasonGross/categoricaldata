package net.categoricaldata.category.Functor
import net.categoricaldata.category._

class CompositeFunctor(val functor1: Functor, val functor2: Functor) extends Functor {
  require(functor1.target == functor2.source)
  val source: functor1.source.type = functor1.source
  val target: functor2.target.type = functor2.target
  def onObjects(o: source.O) = functor2(functor1(o).asInstanceOf[functor2.source.O])
  def onMorphisms(m: source.M) = functor2(functor1(m).asInstanceOf[functor2.source.M])
}
