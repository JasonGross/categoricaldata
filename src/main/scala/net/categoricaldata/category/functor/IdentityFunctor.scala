package net.categoricaldata.category.functor
import net.categoricaldata.category._

class IdentityFunctor(val category: Category) extends Functor {
  val source: category.type = category
  val target: category.type = category
  def onObjects(o: category.O) = o
  def onMorphisms(m: category.M) = m
}

