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

  abstract class FunctorsToSet extends net.metaphor.api.FunctorsToSet[C, F, T, CSets](self) { functorsToSet: CSets => }

  trait FunctorTo[SC <: SmallCategory[SC]] extends SmallHeteroFunctor[SC, C] {
    override val target = self
  }
  trait NaturalTransformationTo[SC <: SmallCategory[SC], F <: FunctorTo[SC]] extends HeteroNaturalTransformation[SC, C, F] {
    override val source: F
    override val target: F
  }

  trait CategoryOver[SC <: SmallCategory[SC], F <: FunctorTo[SC]] {
    def category: SC
    def functor: F
  }
  trait FunctorOver[SC <: SmallCategory[SC], F1 <: Functor[SC], F2 <: FunctorTo[SC], CO <: CategoryOver[SC, F2]] {
    def source: CO
    def target: CO
    def functor: F1
  }

  trait CategoriesOver[SC <: SmallCategory[SC], F1 <: Functor[SC], F2 <: FunctorTo[SC], CO <: CategoryOver[SC, F2], FO <: FunctorOver[SC, F1, F2, CO], CsO <: CategoriesOver[SC, F1, F2, CO, FO, CsO]] extends LargeCategory[CsO] { categoriesOver: CsO =>
    override type O = CO
    override type M = FO
    override def identity(f: CO) = lift(f, f, new Functor.IdentityFunctor(f.category))
    override def source(t: FO) = t.source
    override def target(t: FO) = t.target
    override def compose(m1: FO, m2: FO): FO = lift(m1.source, m2.target, new Functor.CompositeFunctor(m1.functor, m2.functor))

    def lift(source: CO, target: CO, f: Functor[SC]): FO
  }
}
trait LargeCategory[C <: LargeCategory[C]] extends Category[C] { self: C => }
