package net.metaphor.json

case class OnMorphism(arrow: Arrow, map: Map[String, String])

case class Dataset(ontology: Ontology, onObjects: Map[String, List[String]], onMorphisms: List[OnMorphism]) {
	def this(dataset: net.metaphor.api.Ontology#Dataset) = this(
	    ontology = new Ontology(dataset.source),
	    onObjects = dataset.source.objects.map(o => o.name -> dataset(o).toStringList).toMap,
	    onMorphisms = dataset.source.allGenerators.map(a => OnMorphism(new Arrow(a), dataset.onGenerators(a).toStringMap))
	)
}