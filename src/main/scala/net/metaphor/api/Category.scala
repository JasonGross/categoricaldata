package net.metaphor.api

trait Category { category =>
  type O
  type M
  def identity(o: O): M
  def source(m: M): O
  def target(m: M): O
  def compose(m1: M, m2: M): M
  
  // and now some convenience methods for composing many morphisms
  def compose(m0: M, ms: M*): M = ms.fold(m0)(compose _)
  def compose(o: O, ms: List[M]): M = {
    ms match {
      case Nil => identity(o)
      case m :: Nil => m
      case h :: t => compose(h, t:_*)
    }
  }

  
  trait FunctorFrom extends Functor {
    override val source: category.type = category
  }
  trait NaturalTransformationFrom extends NaturalTransformation {
    override val source: FunctorFrom
    override val target: FunctorFrom
  }
  trait FunctorTo extends Functor {
    override val target: category.type = category
  }
  trait NaturalTransformationTo extends NaturalTransformation {
    override val source: FunctorTo
    override val target: FunctorTo
  }

  trait Opposite extends Category {
    type O = category.O
    type M = category.M
    override def identity(o: category.O) = category.identity(o)
    override def source(m: category.M) = category.target(m)
    override def target(m: category.M)=  category.source(m)
    override def compose(m1: category.M, m2: category.M)= category.compose(m2, m1)
  }
  
}
