package net.metaphor.api2

// a container for categories of a particular type (e.g. finitely presented categories)
trait Categories { CAT =>
  type C <: Category

  def identity0(category: C): category.EndoFunctors.Functor = new category.EndoFunctors.Functor {
    override def onObjects(o: SO): TO = o.asInstanceOf[TO]
    override def onMorphisms[X <: SO, Y <: SO](m: SM[X, Y]): TM[TO, TO] = m.asInstanceOf[TM[TO, TO]]
  }
  def identity1(functor: Functor): NaturalTransformation = new NaturalTransformation {
    val sourceAndTargetCategories = new Functors {
      val source = functor.source
      val target = functor.target
    }
    val internalFunctor = sourceAndTargetCategories.Functor(functor)
    val source = internalFunctor
    val target = internalFunctor
    override def apply(o: SO) = functor.target.identity(internalFunctor(o.asInstanceOf[internalFunctor.SO]).asInstanceOf[functor.target.O]).asInstanceOf[TM[TO, TO]]
  }

  def compose(first: Functor, second: Functor): Functor = new Functor {
    val source = first.source
    val target = second.target
    override def onObjects(o: SO) = second(first(o.asInstanceOf[first.SO]).asInstanceOf[second.SO]).asInstanceOf[TO]
    override def onMorphisms[X <: SO, Y <: SO](m: SM[X, Y]) = second(first(m.asInstanceOf[first.SM[first.SO, first.SO]]).asInstanceOf[second.SM[second.SO, second.SO]]).asInstanceOf[TM[TO, TO]]
  }
  def compose1(first: NaturalTransformation, second: NaturalTransformation): NaturalTransformation = ???
  def compose2(first: NaturalTransformation, second: NaturalTransformation): NaturalTransformation = ???

  // a category of type C
  trait Category extends net.metaphor.api2.Category { c: C =>
    type C = CAT.C
    trait FunctorsFrom extends super.FunctorsFrom with CAT.FunctorsFrom
    trait FunctorsTo extends super.FunctorsTo with CAT.FunctorsTo
    object EndoFunctors extends CAT.Functors {
      override val source = c
      override val target = c
      trait EndoFunctor extends Functor
    }
  }

  // a functor between two categories of the same type, C
  trait Functor extends net.metaphor.api2.Functor {
    override type SC = C
    override type TC = C
  }

  // all functors from a particular source to another category of the same type
  trait FunctorsFrom extends net.metaphor.api2.FunctorsFrom { ff =>
    override type SC = C
    val source: C

    trait FunctorFrom extends Functor {
      override val source = ff.source
    }
  }

  // all functors to a particular target from another category of the same type
  trait FunctorsTo extends net.metaphor.api2.FunctorsTo { ft =>
    override type TC = C
    val target: C

    trait FunctorTo extends Functor {
      override val target = ft.target
    }
  }

  // all functors between a specified source and target
  trait Functors extends FunctorsFrom with FunctorsTo with net.metaphor.api2.Functors

  trait NaturalTransformation extends net.metaphor.api2.NaturalTransformation {
    //    override type SC = C
    //    override type TC = C
  }
}
