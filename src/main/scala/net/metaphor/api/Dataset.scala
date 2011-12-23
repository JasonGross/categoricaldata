package net.metaphor.api

trait Dataset extends FunctorToSet[Box, Path, Ontology] {
  override def equals(other: Any): Boolean = {
    other match {
      case other: Dataset => {
        if (source != other.source) return false
        for (o <- source.objects) if (this(o) != other(o)) return false
        for (
          g <- source.allGenerators;
          g1 = this(g).toFunction;
          g2 = other(g).toFunction;
          x <- this(g.source).toIterable
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
      "  onMorphisms = Map(\n" + (for (g <- source.allGenerators; g1 = this(g).toFunction) yield "    " + g.toString + " -> " + (g + (for (x <- this(g.source).toIterable) yield x -> g1(x)).toMap.toString)).mkString("\n") + "  )\n)"
  }
}
