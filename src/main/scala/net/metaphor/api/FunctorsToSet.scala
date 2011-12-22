package net.metaphor.api

abstract class FunctorCategory[SO, SM, SC <: Category[SO, SM, SC], TO, TM, TC <: Category[TO, TM, TC], F <: HeteroFunctor[SO, SM, SC, TO, TM, TC], T<: HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC, F], FC <: FunctorCategory[SO, SM, SC, TO,TM, TC, F, T, FC]](source: SC, target: TC) extends Category[F, T, FC] { self: FC =>
  override def identity(o: F) = lift(new NaturalTransformation.IdentityHeteroNaturalTransformation(o))
  override def source(m: T) = {
    m.source
  }
  override def target(m: T) = {
    m.target
  }
  override def compose(m1: T, m2: T) = lift(new HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC, F] {
    def source = m1.source
    def target = m2.target

    // FIXME code smell; this is duplicated in Composite2NaturalTransformation.apply
    def apply(o: SO) = target.target.compose(m1(o), m2(o))
  })
  
  def lift(t: HeteroNaturalTransformation[SO, SM, SC, TO, TM, TC,F]): T
}

trait FunctorToSet[O, M, C <: Category[O, M, C]] extends HeteroFunctor[O, M, C, Set, Function, Sets] {
  override val target = Sets
}
trait NaturalTransformationToSet[O, M, C <: Category[O, M, C], F <: FunctorToSet[O, M, C]] extends HeteroNaturalTransformation[O, M, C, Set, Function, Sets, F] {
  override def source: F
  override def target: F
}


abstract class FunctorsToSet[O, M, C <: Category[O, M, C], F <: C#FunctorToSet, T <: C#NaturalTransformationToSet[F], FC <: FunctorsToSet[O, M, C, F, T, FC]](source: C) extends FunctorCategory[O, M, C, Set, Function, Sets, F, T, FC](source, Sets) { self: FC => }

//class PullbackFunctor[O1, M1, C1 <: Category[O1, M1, C1], O2, M2, C2 <: Category[O2, M2, C2]](F: Functor[O1, M1, C1], targetCategory: C2) extends Functor[HeteroFunctor[O1, M1, C1, O2, M2, C2], HeteroNaturalTransformation[O1, M1, C1, O2, M2, C2], FunctorCategory[O1, M1, C1, O2, M2, C2]] {
//  val source = new FunctorCategory[O1, M1, C1, O2, M2, C2](F.source, targetCategory)
//  val target = new FunctorCategory[O1, M1, C1, O2, M2, C2](F.target, targetCategory)
//
//  def onObjects(G: HeteroFunctor[O1, M1, C1, O2, M2, C2]) = new Categories.CompositeHeteroFunctor(F, G)
//  def onMorphisms(T: HeteroNaturalTransformation[O1, M1, C1, O2, M2, C2]) = ???
//}
//class PullbackNaturalTransformation[O1, M1, C1 <: Category[O1, M1, C1], O2, M2, C2 <: Category[O2, M2, C2]](T: NaturalTransformation[O1, M1, C1], targetCategory: C2) extends NaturalTransformation[HeteroFunctor[O1, M1, C1, O2, M2, C2], HeteroNaturalTransformation[O1, M1, C1, O2, M2, C2], FunctorCategory[O1, M1, C1, O2, M2, C2]] {
//  val source = new PullbackFunctor[O1, M1, C1, O2, M2, C2](T.source, targetCategory)
//  val target = new PullbackFunctor[O1, M1, C1, O2, M2, C2](T.target, targetCategory)
//
//  def apply(o: HeteroFunctor[O1, M1, C1, O2, M2, C2]) = ???
//}
//
//trait CategoricalTwoFunctor[O1, M1, C1 <: Category[O1, M1, C1], O2, M2, C2 <: Category[O2, M2, C2]] extends HeteroTwoFunctor[C1, Functor[O1, M1, C1], NaturalTransformation[O1, M1, C1], Categories[O1, M1, C1], C2, Functor[O2, M2, C2], NaturalTransformation[O2, M2, C2], Categories[O2, M2, C2]]
//
