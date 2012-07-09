object AbstractMethodErrorBug extends App {

  // Yikes, this still doesn't capture it.
  // c.f. the comment about AbstractMethodError in FinitelyGeneratedCategory.scala 
  
  trait Category
  trait FinitelyGeneratedCategory extends Category {
    type G
  }

  trait Functor {
    val source: Category
    def x
  }

  trait FinitelyGeneratedFunctor extends Functor {
    override val source: FinitelyGeneratedCategory
    def y(g: source.G)
    def g: source.G
    override def x { y(g) }
  }

  trait C {
    val inner: Category
    trait F extends Functor {
      override val source: inner.type = inner
    }
  }

  trait D extends C {
    override val inner: FinitelyGeneratedCategory
    trait F extends super.F with FinitelyGeneratedFunctor {
      //      override val source: inner.type = inner
    }
  }

  val c = new FinitelyGeneratedCategory { type G = Int }
  val e = new D {
    override val inner = c
    trait F extends super.F {
      override val g = 1
      override def y(g: Int) { println(g) }
    }
    val f = new F { }
  }
  e.f.x
}