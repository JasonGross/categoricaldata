package net.metaphor.api

trait Dataset extends FunctorToSet[Ontology] {
  override def equals(other: Any): Boolean = {
    other match {
      case other: Dataset => {
        if (source != other.source) return false
        for (o <- source.objects) if (this(o) != other(o)) return false
        for (
          g <- source.allGenerators;
          m = source.generatorAsMorphism(g);
          g1 = this(m).toFunction;
          g2 = other(m).toFunction;
          x <- this(m.source).toIterable
        ) {
          if (g1(x) != g2(x)) return false
        }
        true
      }
      case _ => false
    }
  }

  override def hashCode = ???
  
  override def toString = {
    "Dataset(\n" +
      "  source = " + source + ", \n" +
      "  onObjects = " + (for (o <- source.objects) yield o -> this(o).toIterable.toList).toMap + ", \n" +
      "  onMorphisms = Map(\n" + (for (g <- source.allGenerators; m = source.generatorAsMorphism(g); g1 = this(m).toFunction) yield "    " + m.toString + " -> " + (m + (for (x <- this(m.source).toIterable) yield x -> g1(x)).toMap.toString)).mkString("\n") + "  )\n)"
  }
  
  // TODO define this recursively, and provide some way to let the user help out. 
  def findIsomorphismsTo(other: Dataset): Iterable[Dataset] = ???
  def isIsomorphicTo(other:Dataset) = findIsomorphismsTo(other).nonEmpty
}
