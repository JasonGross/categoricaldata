package net.metaphor.api

trait Functor {
  val source: Category
  val target: Category

  final def apply(o: source.O): target.O = onObjects(o)
  // the dummy implicit argument is a hack to allow overloading of apply
  final def apply(m: source.M)(implicit d: DummyImplicit): target.M = onMorphisms(m)

  def onObjects(o: source.O): target.O
  def onMorphisms(m: source.M): target.M
}

object Functor {
  class IdentityFunctor(val category: Category) extends Functor {
    val source: category.type = category
    val target: category.type = category
    def onObjects(o: category.O) = o
    def onMorphisms(m: category.M) = m
  }

  class CompositeFunctor(val functor1: Functor, val functor2: Functor) extends Functor {
    require(functor1.target == functor2.source)
    val source: functor1.source.type = functor1.source
    val target: functor2.target.type = functor2.target
    def onObjects(o: source.O) = functor2(functor1(o).asInstanceOf[functor2.source.O])
    def onMorphisms(m: source.M) = functor2(functor1(m).asInstanceOf[functor2.source.M])

  }
}

trait Adjunction {
  val C: Category
  val D: Category
  
  trait LeftAdjoint extends Functor {
    override val source: D.type = D
    override val target: C.type = C
  }
  trait RightAdjoint extends Functor {
    override val source: C.type = C
    override val target: D.type = D
  }
  
  val leftAdjoint: LeftAdjoint
  val rightAdjoint: RightAdjoint
  
  trait Unit extends NaturalTransformation {
    override val source: D.Identity.type = D.Identity
//    override val target = compose(rightAdjoint, leftAdjoint)
  }
  trait Counit extends NaturalTransformation {
//    override val source = compose(leftAdjoint, rightAdjoint)
    override val target: C.Identity.type = C.Identity
  }
  
  val unit: Unit
  val counit: Counit
}

trait MemoFunctor extends Functor {
  import net.tqft.toolkit.functions.Memo
  val memoOnObjects = Memo(super.onObjects _)
  val memoOnMorphisms = Memo(super.onMorphisms _)
  abstract override def onObjects(o: source.O) = memoOnObjects(o)
  abstract override def onMorphisms(o: source.M) = memoOnMorphisms(o)
}
