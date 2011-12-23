package net.metaphor.api

trait Category[O, M, C <: Category[O, M, C]] { self: C =>
  def identity(o: O): M
  def source(m: M): O
  def target(m: M): O
  def compose(m1: M, m2: M): M

  trait FunctorFrom[TO, TM, TC <: Category[TO, TM, TC]] extends HeteroFunctor[O, M, C, TO, TM, TC] {
    override val source = self
  }
  trait NaturalTransformationFrom[TO, TM, TC <: Category[TO, TM, TC], F <: FunctorFrom[TO, TM, TC]] extends HeteroNaturalTransformation[O, M, C, TO, TM, TC, F] {
    override def source: F
    override def target: F
  }
  trait FunctorTo[SO, SM, SC <: Category[SO, SM, SC]] extends HeteroFunctor[SO, SM, SC, O, M, C] {
    override val target = self
  }
  trait NaturalTransformationTo[SO, SM, SC <: Category[SO, SM, SC], F <: FunctorTo[SO, SM, SC]] extends HeteroNaturalTransformation[SO, SM, SC, O, M, C, F] {
    override def source: F
    override def target: F
  }

  trait CategoryOver[SO, SM, SC <: Category[SO, SM, SC], F <: FunctorTo[SO, SM, SC]] {
    def category: SC
    def functor: F
  }
  trait FunctorOver[SO, SM, SC <: Category[SO, SM, SC], F1 <: Functor[SO, SM, SC], F2 <: FunctorTo[SO, SM, SC], CO <: CategoryOver[SO, SM, SC, F2]] { 
    def source: CO
    def target: CO
    def functor: F1
  }
  
  abstract class CategoriesOver[SO, SM, SC <: Category[SO, SM, SC], F1 <: Functor[SO, SM, SC], F2 <: FunctorTo[SO,SM,SC], CO <: CategoryOver[SO, SM, SC, F2], FO <: FunctorOver[SO, SM, SC, F1, F2, CO], D <: CategoriesOver[SO, SM, SC, F1, F2,  CO, FO, D]] extends Category[CO, FO, D] { categoriesOver: D =>
    override def identity(f: CO) = lift(new Functor.IdentityFunctor(f.category))
    override def source(t: FO) = t.source
    override def target(t: FO) = t.target
    override def compose(m1: FO, m2: FO): FO = lift(new Functor.CompositeFunctor(m1.functor, m2.functor))
    
    def lift(f: Functor[SO, SM, SC]): FO
  }
  
  trait FunctorToSet extends FunctorFrom[Set, Function, Sets] with net.metaphor.api.FunctorToSet[O, M, C] 

  trait NaturalTransformationToSet[F <: FunctorToSet] extends NaturalTransformationFrom[Set, Function, Sets, F] with net.metaphor.api.NaturalTransformationToSet[O, M, C, F] {
    override def source: F
    override def target: F
  }

  abstract class FunctorsToSet[F <: FunctorToSet, T <: NaturalTransformationToSet[F], FC <: FunctorsToSet[F, T, FC]] extends net.metaphor.api.FunctorsToSet[O, M, C, F, T, FC](self) { functorsToSet: FC => }
}