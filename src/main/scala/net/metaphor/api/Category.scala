package net.metaphor.api

trait FPCategory {
  val boxes: List[Box]
  val arrows: List[Arrow]
  val constraints: List[Constraint] = Nil

  def describe = arrows.map(_.describe).mkString("\n")

  case class Box(label: String, description: String = "")
  case class Arrow(source: Box, target: Box, label: String, description: String = "") {
    def describe = source.description + " " + description + " " + target.description
  }

  implicit def arrowAsPath(arrow: Arrow) = Path(arrow.source, List(arrow))

  case class Path(initialBox: Box, arrows: List[Arrow]) {
    require(arrows.isEmpty || arrows.head.source == initialBox)
    // TODO check sources and targets match
    def finalBox = if (arrows.isEmpty) initialBox else arrows.last.target
  }
  case class Constraint(left: Path, right: Path) {
    // TODO check something
  }
}

sealed trait Star extends FPCategory

object Star {
  def apply(n: Int): Star = apply(n, "id")
  def apply(n: Int, id: String): Star = apply(n, id, (1 to n).toList.map("attr:" + _))
  def apply(n: Int, columns: List[String]): Star = apply(n, "id", columns)
  def apply(n: Int, id: String, columns: List[String]): Star = new Star {
    val centre = Box(id)
    val leaves = columns.map(c => Box(c))
    val boxes = centre :: leaves
    val arrows = leaves map { c => Arrow(centre, c, c.label) }
  }
}

trait Category[O, M] {
  def identity(o: O): M
}

trait Set
trait Function

object Sets extends Category[Set, Function] {
	def identity(set: Set) = ???
}

object FunctorsTo {
  def apply[O1, M1 <: Morphism, O2, M2 <: Morphism](category: Category[O2, M2]): Category[Functor[O1, M1, O2, M2], NaturalTransformation[O1, M1, O2, M2]] = new Category[Functor[O1, M1, O2, M2], NaturalTransformation[O1, M1, O2, M2]] {
    def identity(functor: Functor[O1, M1, O2, M2]) = new IdentityNaturalTransformation(functor)
  }
}

trait FinitelyGeneratedCategory[O, M] extends Category[O, M] {
  def objects: List[O]
  def generators(source: O, target: O): List[M]
  def generators: List[M]
}

trait FinitelyPresentedCategory[O, M] extends FinitelyGeneratedCategory[O, M] {
	
}

// Morphism just exists to provide an upper bound, to disambiguate apply(object) and apply(morphism) after type erasure.
trait Morphism

trait Functor[O1, M1 <: Morphism, O2, M2 <: Morphism] {
  def source: Category[O1, M1]
  def target: Category[O2, M2]

  def apply(o: O1): O2
  def apply(m: M1): M2
}

class IdentityFunctor[O, M <: Morphism](category: Category[O, M]) extends Functor[O, M, O, M] {
  def source = category
  def target = category
  def apply(o: O) = o
  def apply(m: M) = m
}

trait NaturalTransformation[O1, M1 <: Morphism, O2, M2 <: Morphism] {
  def source: Functor[O1, M1, O2, M2]
  def target: Functor[O1, M1, O2, M2]
  def apply(o: O1): M2
}

class IdentityNaturalTransformation[O1, M1 <: Morphism, O2, M2 <: Morphism](functor: Functor[O1, M1, O2, M2]) extends NaturalTransformation[O1, M1, O2, M2] {
  def source = functor
  def target = functor
  def apply(o: O1) = functor.target.identity(functor(o))
}


trait Box {
  def name: String
  def description: String
}

trait Arrow {
  def source: Box
  def target: Box
  def name: String
  def description: String
}

trait Path extends Morphism {
  def start: Box
  def end: Box
  def arrows: List[Arrow]
}

trait Ontology extends FinitelyGeneratedCategory[Box, Path] {

}

trait Translation extends Functor[Box, Path, Box, Path]

trait Model {
  val over: Ontology
}

trait PolynomialFunctor {
  def apply(model: Model): Model
}

trait TwoCategory[M0, M1, M2] {
  def identity0(m0: M0): M1
  def identity1(m1: M1): M2
}

class Categories[O, M <: Morphism, C <: Category[O, M]] extends TwoCategory[C, Functor[O, M, O, M], NaturalTransformation[O, M, O, M]] {
  def identity0(category: C) = new IdentityFunctor[O, M](category)
  def identity1(functor: Functor[O, M, O, M]) = new IdentityNaturalTransformation[O, M, O, M](functor)
}
class FinitelyPresentedCategories[O, M <: Morphism] extends Categories[O, M, FinitelyPresentedCategory[O, M]]

trait RigidTwoCategory[M0, M1, M2] extends TwoCategory[M0, M1, M2] {
  def leftDual(m1: M1): M1
  def leftPairing(m1: M1): M2
  def leftCopairing(m1: M1): M2
  def rotateLeft(m2: M2): M2

  def rightDual(m1: M1): M1
  def rightPairing(m1: M1): M2
  def rightCopairing(m1: M1): M2
  def rotateRight(m2: M2): M2
}

trait TwoCategoryOfCategories[O, M <: Morphism, C <: Category[O, M], F <: Functor[O, M, O, M]] extends TwoCategory[C, F, NaturalTransformation[_, _, _, _]]

trait TwoCategoryOfFinitelyPresentedCategories[O, M <: Morphism] extends TwoCategoryOfCategories[O, M, FinitelyPresentedCategory[O, M], Functor[O, M, O, M]]

trait FPFunctor {
  val source: FPCategory
  val target: FPCategory

  def boxMap: source.Box => target.Box
  def arrowMap: source.Arrow => target.Path

  def apply(box: source.Box): target.Box = boxMap(box)
  def apply(arrow: source.Arrow): target.Path = arrowMap(arrow)
  def apply(path: source.Path): target.Path = {
    target.Path(apply(path.initialBox), path.arrows.flatMap(apply(_).arrows))
  }

  def verify {
    for (arrow <- source.arrows) {
      require(apply(arrow.source) == apply(arrow).initialBox)
      require(apply(arrow.target) == apply(arrow).finalBox)
    }

    // TODO think about how to check constraints
    // TODO possibly, require a proof?
  }
}

trait FunctorToSet {
  val source: FPCategory
  def precompose(functor: FPFunctor): FunctorToSet
  def realize: ConcreteFunctorToSet
}

trait ConcreteFunctorToSet extends FunctorToSet { F =>
  def apply(box: source.Box): List[_]
  def apply(arrow: source.Arrow): List[(_, _)]

  def realize = this

  // a lazy implementation of precompose
  override def precompose(G: FPFunctor): ConcreteFunctorToSet = new ConcreteFunctorToSet {
    require(G.target == F.source)
    val source = G.source
    // we need to coerce source.Box to G.source.Box, and G.target.Box to F.source.Box, because the typer can't know these are the same.
    def apply(box: source.Box) = F.apply(G.apply(box.asInstanceOf[G.source.Box]).asInstanceOf[F.source.Box])
    // similarly for Paths
    def apply(arrow: source.Arrow) = F.apply(G.apply(arrow.asInstanceOf[G.source.Arrow]).asInstanceOf[F.source.Arrow])
  }
}

abstract class InMemoryFunctorToSet extends ConcreteFunctorToSet {
  val boxData: Map[source.Box, List[_]]
  val arrowData: Map[source.Arrow, List[(_, _)]]

  def apply(box: source.Box) = boxData(box)
  def apply(arrow: source.Arrow) = arrowData(arrow)
}

