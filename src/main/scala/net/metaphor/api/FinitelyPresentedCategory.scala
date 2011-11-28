package net.metaphor.api

trait FinitelyGeneratedCategory[O, M] extends Category[O, M] {
  def objects: List[O]
  def generators(source: O, target: O): List[M]
  def generators: List[M] = for (s <- objects; t <- objects; g <- generators(s, t)) yield g
}

object FinitelyPresentedCategory {
  trait Initial
  trait Terminal

  case class InitialMorphism[O](to: Either[O, Initial])
  case class TerminalMorphism[O](from: Either[O, Terminal])
}

trait FinitelyPresentedCategory[O, M] extends FinitelyGeneratedCategory[O, M] { self =>
  def relations(source: O, target: O): List[M]
  def relations: List[M] = for (s <- objects; t <- objects; r <- relations(s, t)) yield r

  import FinitelyPresentedCategory._

  def withTerminalObject: (FinitelyPresentedCategory[Either[O, Terminal], Either[M, TerminalMorphism[O]]], Terminal) = {
    object * extends Terminal

    val augmentedCategory = new FinitelyPresentedCategory[Either[O, Terminal], Either[M, TerminalMorphism[O]]] {
      override def objects = Right[O, Terminal](*) :: self.objects.map(Left[O, Terminal](_))
      override def generators(source: Either[O, Terminal], target: Either[O, Terminal]) = ???
      override def relations(source: Either[O, Terminal], target: Either[O, Terminal]) = ???

      override def compose(m1: Either[M, TerminalMorphism[O]], m2: Either[M, TerminalMorphism[O]]) = ???
      override def source(m: Either[M, TerminalMorphism[O]]) = {
        m match {
          case Left(n) => Left(self.source(n))
          case Right(TerminalMorphism(from)) => from
        }
      }
      override def target(m: Either[M, TerminalMorphism[O]]) = {
        m match {
          case Left(n) => Left(self.target(n))
          case Right(TerminalMorphism(_)) => Right(*)
        }
      }
      override def identity(o: Either[O, Terminal]) = {
        o match {
          case Left(u) => Left(self.identity(u))
          case Right(_) => Right(TerminalMorphism(Right(*)))
        }
      }
    }

    (augmentedCategory, *)
  }
}

trait FinitelyPresentedCategories[O, M, C <: FinitelyPresentedCategory[O, M]] extends Categories[O, M, C]

