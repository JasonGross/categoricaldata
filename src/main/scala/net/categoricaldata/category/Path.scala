package net.categoricaldata.category

case class Path[O, G](source: O, target: O, morphisms: List[G]) {
  if (morphisms.isEmpty) require(source == target)

  def length = morphisms.size
  def andThen(path: Path[O, G]) = {
    require(target == path.source)
    Path(source, path.target, morphisms ::: path.morphisms)
  }

  // This is purely a micro-optimization.
  override lazy val hashCode = morphisms.hashCode

  override def toString = {
    def generatorToString(g: G): String = {
      g match {
        case g: net.categoricaldata.ontology.Arrow => " --- \"" + g.name + "\" --> " + g.target.toString
        case _ => "(" + g.toString() + ")"
      }
    }

    source.toString + morphisms.map(generatorToString(_)).mkString
  }
}
