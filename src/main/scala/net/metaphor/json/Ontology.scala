package net.metaphor.json

case class Arrow(source: String, target: String, label: String)

case class Relation(left: List[Arrow], right: List[Arrow])

case class Ontology(objects: List[String], arrows: List[Arrow], relations: List[Relation]) {
  def unpack: net.metaphor.api.Ontology = {
    val stringArrows = arrows.map(a => net.metaphor.dsl.Sentences.StringArrow(a.source, a.target, a.label))
    val stringRelations = relations.map({
      case Relation(left, right) => {
        val source = left.headOption.getOrElse(right.head).source
        net.metaphor.dsl.Sentences.StringRelation(
          net.metaphor.dsl.Sentences.ConcreteStringPath(source, left.map(a => net.metaphor.dsl.Sentences.StringArrow(a.source, a.target, a.label))),
          net.metaphor.dsl.Sentences.ConcreteStringPath(source, right.map(a => net.metaphor.dsl.Sentences.StringArrow(a.source, a.target, a.label))))
      }

    })
    net.metaphor.dsl.Sentences.Ontology(objects, stringArrows, stringRelations)
  }
}