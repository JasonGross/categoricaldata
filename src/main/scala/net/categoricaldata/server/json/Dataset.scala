package net.categoricaldata.server.json

case class DatasetOnArrow(arrow: Arrow, map: Map[String, String])

case class Dataset(ontology: Ontology, onObjects: Map[String, List[String]], onMorphisms: List[DatasetOnArrow], json: Option[String] = None) {
  def unpack: net.categoricaldata.ontology.Ontology#Dataset = {
    net.categoricaldata.ontology.Dataset(
        ontology.unpack,
        onObjects,
        onMorphisms.map({ case DatasetOnArrow(a, map) => net.categoricaldata.dsl.Sentences.StringArrow(a.source, a.target, a.label) -> map }).toMap,
        json
    )
  }
}