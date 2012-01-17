package net.categoricaldata.api

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
      case h :: t => compose(h, t: _*)
    }
  }
  def power(m: M, k: Int): M = {
    require(source(m) == target(m))
    k match {
      case 0 => identity(source(m))
      case 1 => m
      case k if k >= 2 => compose(source(m), List.fill(k)(m))
      case k if k < 0 => throw new IllegalArgumentException
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
  trait EndoFunctor extends FunctorFrom with FunctorTo
  
  trait Identity extends EndoFunctor {
    override def onObjects(o: O) = o
    override def onMorphisms(m: M) = m
  }
  object identityFunctor extends Identity
 }
