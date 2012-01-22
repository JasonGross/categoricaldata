package net.categoricaldata.server.json

case class Arrow(source: String, target: String, label: String)

case class Relation(left: List[Arrow], right: List[Arrow])

case class Ontology(objects: List[String], arrows: List[Arrow], relations: List[Relation], provenance: Option[Provenance] = None, json: Option[String] = None) extends JSONPacket {
  def unpack: net.categoricaldata.ontology.Ontology = {
    val stringArrows = arrows.map(a => net.categoricaldata.dsl.Sentences.StringArrow(a.source, a.target, a.label))
    val stringRelations = relations.map({
      case Relation(left, right) => {
        val source = left.headOption.getOrElse(right.head).source
        net.categoricaldata.dsl.Sentences.StringRelation(
          net.categoricaldata.dsl.Sentences.ConcreteStringPath(source, left.map(a => net.categoricaldata.dsl.Sentences.StringArrow(a.source, a.target, a.label))),
          net.categoricaldata.dsl.Sentences.ConcreteStringPath(source, right.map(a => net.categoricaldata.dsl.Sentences.StringArrow(a.source, a.target, a.label))))
      }

    })
    net.categoricaldata.ontology.Ontology(objects, stringArrows, stringRelations, json)
  }

  override def updateProvenance(_provenance: Provenance) = copy(provenance = Some(_provenance))

}