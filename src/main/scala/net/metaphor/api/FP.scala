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

trait FPFunctorToSet {
  val source: FPCategory
  def precompose(functor: FPFunctor): FPFunctorToSet
  def realize: ConcreteFPFunctorToSet
}

trait ConcreteFPFunctorToSet extends FPFunctorToSet { F =>
  def apply(box: source.Box): List[_]
  def apply(arrow: source.Arrow): List[(_, _)]

  def realize = this

  // a lazy implementation of precompose
  override def precompose(G: FPFunctor): ConcreteFPFunctorToSet = new ConcreteFPFunctorToSet {
    require(G.target == F.source)
    val source = G.source
    // we need to coerce source.Box to G.source.Box, and G.target.Box to F.source.Box, because the typer can't know these are the same.
    def apply(box: source.Box) = F.apply(G.apply(box.asInstanceOf[G.source.Box]).asInstanceOf[F.source.Box])
    // similarly for Paths
    def apply(arrow: source.Arrow) = F.apply(G.apply(arrow.asInstanceOf[G.source.Arrow]).asInstanceOf[F.source.Arrow])
  }
}

abstract class InMemoryFPFunctorToSet extends ConcreteFPFunctorToSet {
  val boxData: Map[source.Box, List[_]]
  val arrowData: Map[source.Arrow, List[(_, _)]]

  def apply(box: source.Box) = boxData(box)
  def apply(arrow: source.Arrow) = arrowData(arrow)
}

