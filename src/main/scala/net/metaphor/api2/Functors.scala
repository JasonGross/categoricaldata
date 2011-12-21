package net.metaphor.api2

// a functors with a particular source (note that the source category also has an inner type representing the same thing, with more type-safety)
trait FunctorsFrom { functorsFrom =>
  type SC <: Category

  val source: SC

  // a functor with a specified source and target
  trait Functor extends net.metaphor.api2.Functor {
    type SC = functorsFrom.SC
    val source = functorsFrom.source
  }
}
// a functors with a particular source (note that the source category also has an inner type representing the same thing, with more type-safety)
trait FunctorsTo { functorsTo =>
  type TC <: Category

  val target: TC

  // a functor with a specified source and target
  trait Functor extends net.metaphor.api2.Functor {
    type TC = functorsTo.TC
    val target = functorsTo.target
  }
}

// all functors with a specified source and target
trait Functors extends FunctorsFrom with FunctorsTo { functors =>
  // a functor with a specified source and target
  trait Functor extends net.metaphor.api2.Functor {
    override type SC = functors.SC
    override type TC = functors.TC
    override val source = functors.source
    override val target = functors.target
  }

  object Functor {
    def apply(functor: net.metaphor.api2.Functor) = {
      require(functor.source == source)
      require(functor.target == target)
      new Functor {
        override def onObjects(o: SO) = functor(o.asInstanceOf[functor.SO]).asInstanceOf[TO]
        override def onMorphisms[X <: SO, Y <: SO](m: SM[X, Y]): TM[TO, TO] = functor.onMorphisms(m.asInstanceOf[functor.SM[functor.SO, functor.SO]]).asInstanceOf[TM[TO, TO]]
      }
    }
  }
}
