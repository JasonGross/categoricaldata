package net.metaphor.api2

trait Categories { CAT =>
  type C <: Category

  trait Category { c: C =>
    type O
    type M

    def identity(o: O): M
    def source(m: M): O
    def target(m: M): O
    def compose(m1: M, m2: M): M

    trait FunctorsFrom extends CAT.FunctorsFrom {
      val source = c
    }
    trait FunctorsTo extends CAT.FunctorsTo {
      val target = c
    }
  }

  trait Functor {
    val source: C
    val target: C

    final def apply(o: source.O): target.O = onObjects(o)
    final def apply(m: source.M)(implicit d: DummyImplicit): target.M = onMorphisms(m)
    
    def onObjects(o: source.O): target.O
    def onMorphisms(m: source.M): target.M
  }

  trait FunctorsFrom { ff =>
    val source: C

    trait FunctorFrom extends Functor {
      override val source = ff.source
    }
  }

  trait FunctorsTo { ft =>
    val target: C

    trait FunctorTo extends Functor {
      override val source = ft.target
    }
  }

  trait FunctorsBetween extends FunctorsFrom with FunctorsTo { fb =>
    trait FunctorBetween extends Functor {
      override val source = fb.source
      override val target = fb.target
    }
  }

  trait NaturalTransformation {
    val sourceAndTarget: FunctorsBetween
    val source: sourceAndTarget.FunctorBetween
    val target: sourceAndTarget.FunctorBetween

    // gosh, wouldn't it be nice if we could have the type system ensure this morphism was really a morphism between source(o) and target(o)
    // ... or is that just going too far?
    def apply(o: sourceAndTarget.source.O): sourceAndTarget.target.M
  }
}
