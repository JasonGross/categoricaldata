package net.categoricaldata.category

trait NaturalTransformation {
  val source: Functor
  val target: Functor
  lazy val sourceCategory: target.source.type = target.source
  lazy val targetCategory: target.target.type = target.target
  def apply(o: sourceCategory.O): targetCategory.M
}

object NaturalTransformation {
  class IdentityNaturalTransformation(val functor: Functor) extends NaturalTransformation {
    val source: functor.type = functor
    val target: functor.type = functor
    override def apply(o: sourceCategory.O) = functor.target.identity(functor(o.asInstanceOf[functor.source.O]))
  }
}