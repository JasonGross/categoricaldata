package net.metaphor.api

trait TypedCategory[O[_], M[_, _]] {
  def identity[A](o: O[A]): M[A, A]
}

trait FinitelyGeneratedTypedCategory[O[_], M[_, _]] extends TypedCategory[O, M] {
  def generators[A, B](source: O[A], target: O[B]): List[M[A, B]]
  def generators[A, B]: List[M[A, B]]
}

trait FinitelyPresentedTypedCategory[O[_], M[_, _]] extends FinitelyGeneratedTypedCategory[O, M] {
  def relations[A, B](source: O[A], target: O[B]): List[(M[A, B], M[A, B])]
  def relations[A, B]: List[(M[A, B], M[A, B])]
}

trait TypedMorphism[+O[_], A, B] {
  def source: O[A]
  def target: O[B]
}

trait TypedFunctor[O1[_], M1[A, B] <: TypedMorphism[O1, A, B], O2[_], M2[A, B] <: TypedMorphism[O2, A, B], Q[_]] {
  def source: TypedCategory[O1, M1]
  def target: TypedCategory[O2, M2]

  def apply[A](o: O1[A]): O2[Q[A]]
  def apply[A, B](m: M1[A, B]): M2[Q[A], Q[B]]
}
