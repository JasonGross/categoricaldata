package net.metaphor.json

object Pack {
  private def packArrow(arrow: net.metaphor.api.Arrow) = Arrow(arrow.source.name, arrow.target.name, arrow.name)

  private def packRelation(pair: (net.metaphor.api.Path[net.metaphor.api.Box, net.metaphor.api.Arrow], net.metaphor.api.Path[net.metaphor.api.Box, net.metaphor.api.Arrow])) = {
    Relation(left = pair._1.morphisms.map(packArrow(_)), right = pair._2.morphisms.map(packArrow(_)))
  }

  implicit def packOntology(ontology: net.metaphor.api.Ontology) = Ontology(
    objects = ontology.objects.map(_.name),
    arrows = ontology.allGenerators.map(packArrow _),
    relations = ontology.allRelations.map(packRelation _))

  implicit def packDataset(dataset: net.metaphor.api.Ontology#Dataset) = Dataset(
    ontology = packOntology(dataset.source),
    onObjects = dataset.source.objects.map(o => o.name -> dataset(o).toStringList).toMap,
    onMorphisms = dataset.source.allGenerators.map(a => DatasetOnArrow(packArrow(a), dataset.onGenerators(a).toStringMap)))

  private def packTranslationOnArrow(arrow: net.metaphor.api.Arrow, pathEquivalenceClass: net.metaphor.api.Ontology#PathEquivalenceClass) = TranslationOnArrow(
    packArrow(arrow),
    pathEquivalenceClass.representative.morphisms.map(packArrow(_)))

  implicit def packTranslation(translation: net.metaphor.api.Translation) = Translation(
    source = Pack.packOntology(translation.source),
    target = Pack.packOntology(translation.target),
    onObjects = translation.source.objects.map(o => o.name -> translation(o).name).toMap,
    onGenerators = translation.source.allGenerators.map(a => packTranslationOnArrow(a, translation.onGenerators(a))))

}
