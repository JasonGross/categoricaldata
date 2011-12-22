package net.metaphor.api

trait Datamap[F <: Dataset] extends NaturalTransformationToSet[Box, Path, Ontology, F]