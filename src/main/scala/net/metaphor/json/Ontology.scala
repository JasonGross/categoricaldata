package net.metaphor.json

case class Arrow(source: String, target: String, label: String) {
  def this(arrow: net.metaphor.api.Arrow) = this(arrow.source.name, arrow.target.name, arrow.name)
}
case class Path(arrows: List[Arrow]) {
  def this(path: net.metaphor.api.Path[net.metaphor.api.Box, net.metaphor.api.Arrow]) = this(path.morphisms.map(new Arrow(_)))
}
case class Relation(left: Path, right: Path) {
  def this(pair: (net.metaphor.api.Path[net.metaphor.api.Box, net.metaphor.api.Arrow], net.metaphor.api.Path[net.metaphor.api.Box, net.metaphor.api.Arrow])) = this(
		  left = new Path(pair._1), right = new Path(pair._2)
  )
}

case class Ontology(objects: List[String], arrows: List[Arrow], relations: List[Relation]) {
  def this(ontology: net.metaphor.api.Ontology) = this(
      objects = ontology.objects.map(_.name),
      arrows = ontology.allGenerators.map(new Arrow(_)),
      relations = ontology.allRelations.map(new Relation(_)))
  def unpack: net.metaphor.api.Ontology = ???
}