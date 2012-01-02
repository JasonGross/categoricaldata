package net.metaphor.json

case class TranslationOnArrow(arrow: Arrow, path: List[Arrow])

case class Translation(source: Ontology, target: Ontology, onObjects: Map[String, String], onGenerators: List[TranslationOnArrow]) {
  def unpack: net.metaphor.api.Translation with net.metaphor.api.FiniteTarget = {
    net.metaphor.dsl.Sentences.Translation(
        source.unpack, 
        target.unpack.assertFinite,
        onObjects,
        onGenerators.map({ case TranslationOnArrow(a, path) =>
          net.metaphor.dsl.Sentences.StringArrow(a.source, a.target, a.label) ->
          net.metaphor.dsl.Sentences.ConcreteStringPath(onObjects(a.source), path.map(a => net.metaphor.dsl.Sentences.StringArrow(a.source, a.target, a.label)))
          }).toMap
    )
  }
}
