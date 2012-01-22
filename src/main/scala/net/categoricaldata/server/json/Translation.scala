package net.categoricaldata.server.json

case class TranslationOnArrow(arrow: Arrow, path: List[Arrow])

case class Translation(
  source: Ontology,
  target: Ontology,
  onObjects: Map[String, String],
  onGenerators: List[TranslationOnArrow],
  provenance: Option[Provenance] = None,
  json: Option[String] = None) extends JSONPacket {
  require(source != null)
  require(target != null)
  require(onObjects != null)
  for (o <- source.objects) require(onObjects.keySet.contains(o))
  require(onGenerators != null)
  for (a <- source.arrows) require(onGenerators.exists(_.arrow == a))

  def unpack: net.categoricaldata.ontology.Translation = {
    net.categoricaldata.ontology.Translation(
      source.unpack,
      target.unpack.assertFinite,
      onObjects,
      onGenerators.map({
        case TranslationOnArrow(a, path) =>
          net.categoricaldata.dsl.Sentences.StringArrow(a.source, a.target, a.label) ->
            net.categoricaldata.dsl.Sentences.ConcreteStringPath(onObjects(a.source), path.map(a => net.categoricaldata.dsl.Sentences.StringArrow(a.source, a.target, a.label)))
      }).toMap,
      json)
  }
  
  override def updateProvenance(_provenance: Provenance) = copy(provenance = Some(_provenance))
  override def fillEmptyJSON = copy(json = Some(json.getOrElse(PrettyPrinter(this))))
}
