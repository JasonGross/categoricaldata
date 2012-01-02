package net.metaphor.json

case class DatasetOnArrow(arrow: Arrow, map: Map[String, String])

case class Dataset(ontology: Ontology, onObjects: Map[String, List[String]], onMorphisms: List[DatasetOnArrow]) {
  def unpack: net.metaphor.api.Ontology#Dataset = {
    net.metaphor.dsl.Sentences.Dataset(
        ontology.unpack,
        onObjects,
        onMorphisms.map({ case DatasetOnArrow(a, map) => net.metaphor.dsl.Sentences.StringArrow(a.source, a.target, a.label) -> map }).toMap
    )
  }
}