package net.metaphor.api

trait Datamap[F <: Dataset] extends NaturalTransformationToSet[Box, Path, Ontology, F] {
    override def equals(other: Any): Boolean = {
      ???
    }
    
    override def hashCode = ???
    
    override def toString = ???
}