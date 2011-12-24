package net.metaphor.api

trait Category[C <: Category[C]] { self: C =>
  type O
  type M
  def identity(o: O): M
  def source(m: M): O
  def target(m: M): O
  def compose(m1: M, m2: M): M

  trait FunctorFrom[TC <: Category[TC]] extends HeteroFunctor[C, TC] {
    override val source: self.type = self
  }
  trait NaturalTransformationFrom[TC <: Category[TC], F <: FunctorFrom[TC]] extends HeteroNaturalTransformation[C, TC, F] {
    override val source: F
    override val target: F
  }
  trait FunctorTo[SC <: Category[SC]] extends HeteroFunctor[SC, C] {
    override val target: self.type = self
  }
  trait NaturalTransformationTo[SC <: Category[SC], F <: FunctorTo[SC]] extends HeteroNaturalTransformation[SC, C, F] {
    override val source: F
    override val target: F
  }

  trait FunctorToSet extends FunctorFrom[Sets] with net.metaphor.api.FunctorToSet[C]

  trait NaturalTransformationToSet[F <: FunctorToSet] extends NaturalTransformationFrom[Sets, F] with net.metaphor.api.NaturalTransformationToSet[C, F] {
    override val source: F
    override val target: F
    override val sourceCategory: self.type = self
  }
}

trait SmallCategory[C <: SmallCategory[C]] extends Category[C] { self: C =>
  type F <: FunctorToSet
  type T <: NaturalTransformationToSet[F]
  type CSets <: FunctorsToSet

  def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[C]): F
  def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[C, F]): T

  def functorsToSet: CSets

  abstract class FunctorsToSet extends net.metaphor.api.FunctorsToSet[C, CSets](self) { functorsToSet: CSets =>
    override type O = self.F
    override type M = self.T
  }

  trait FunctorTo[SC <: SmallCategory[SC]] extends SmallHeteroFunctor[SC, C] {
    override val target = self
  }
  trait NaturalTransformationTo[SC <: SmallCategory[SC], F <: FunctorTo[SC]] extends HeteroNaturalTransformation[SC, C, F] {
    override val source: F
    override val target: F
  }

  trait CategoryOver[SC <: SmallCategory[SC]] {
    def category: SC
    def functor: FunctorTo[SC]
  }
  trait FunctorOver[SC <: SmallCategory[SC]] {
    def source: CategoryOver[SC]
    def target: CategoryOver[SC]
    def functor: Functor[SC]
  }

  trait CategoriesOver[SC <: SmallCategory[SC]] extends LargeCategory[CategoriesOver[SC]] { categoriesOver =>
    override type O = CategoryOver[SC]
    override type M = FunctorOver[SC]
    override def identity(f: O) = new FunctorOver[SC]{
      val source =f
      val target = f
      var functor= new Functor.IdentityFunctor(f.category)
    }
    override def source(t: M) = t.source
    override def target(t: M) = t.target
    override def compose(m1: M, m2: M): M = new FunctorOver[SC] {
      val source = m1.source
      val target = m2.target
      val functor = new Functor.CompositeFunctor(m1.functor, m2.functor)
    }
  }
  
  def categoriesOver[SC <: SmallCategory[SC]] = new CategoriesOver[SC] { }
}


trait LargeCategory[C <: LargeCategory[C]] extends Category[C] { self: C => }
