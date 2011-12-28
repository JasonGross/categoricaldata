package net.metaphor.api

trait NaturalTransformation {
  val source: Functor
  val target: Functor
  val sourceCategory: source.source.type = source.source
  val targetCategory: source.target.type = source.target
  def apply(o: sourceCategory.O): targetCategory.M
}

object NaturalTransformation {
  class IdentityNaturalTransformation(val functor: Functor) extends NaturalTransformation {
    val source: functor.type = functor
    val target: functor.type = functor
    override def apply(o: sourceCategory.O) = functor.target.identity(functor(o.asInstanceOf[functor.source.O])).asInstanceOf[targetCategory.M]
  }
}