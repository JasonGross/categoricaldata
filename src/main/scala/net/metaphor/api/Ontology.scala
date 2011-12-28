package net.metaphor.api

case class Box(name: String) {
  override def toString = "\"" + name + "\""
}

case class Arrow(source: Box, target: Box, name: String) {
  override def toString = source.toString + " --- \"" + name + "\" --> " + target.toString
}

trait Ontology extends FinitelyPresentedCategory { ontology =>
  override type O = Box
  override type G = Arrow

  override def generatorSource(g: G) = g.source
  override def generatorTarget(g: G) = g.target

  // TODO pull this up
  override def equals(other: Any) = {
    other match {
      case other: Ontology => {
        objects == other.objects && allGenerators == other.allGenerators && allRelations == other.allRelations
      }
      case _ => false
    }
  }

  override def hashCode = ???

  override def toString = {
    // TODO relations in Ontology.toString
    "Ontology(objects = " + (for (o <- objects) yield o.name) + ", arrows = " + allGenerators + ")"
  }

  trait Dataset extends FunctorToSet { dataset =>
    override def equals(other: Any): Boolean = {
      other match {
        case other: Ontology#Dataset => {
          if (dataset.source != other.source) return false
          for (o <- source.objects) if (this(o) != other(o)) return false
          for (
            g <- source.allGenerators;
            m1 = source.generatorAsMorphism(g);
            m2 = other.source.generatorAsMorphism(g);
            g1 = this(m1).toFunction;
            g2 = other(m2).toFunction;
            x <- this(source.source(g)).toIterable
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
        "  onMorphisms = Map(" + (if (source.allGenerators.nonEmpty) "\n" else "") +
        (for (
          g <- source.allGenerators;
          m = source.generatorAsMorphism(g);
          g1 = this(m).toFunction
        ) yield {
          "    " + m.toString + " -> " + (m + (for (x <- this(source.source(m)).toIterable) yield x -> g1(x)).toMap.toString)
        }).mkString("\n") + "  ))"
    }

    // TODO define this recursively, and provide some way to let the user help out. 
    def findIsomorphismsTo(other: Ontology#Dataset): Iterable[Datamap] = {
      ???
    }

    def isIsomorphicTo(other: Ontology#Dataset) = findIsomorphismsTo(other).nonEmpty

  }
  trait Datamap extends NaturalTransformationToSet

  override type F = Dataset
  override type T = Datamap

  override def internalize(f: net.metaphor.api.FunctorToSet) = {
    f match {
      case f: Dataset => f
      case _ => {
        require(f.source == this)
        new Dataset {
          override def onObjects(o: Box) = f(o.asInstanceOf[f.source.O])
          // and yet another weird one: replacing this with Arrow causes an AbstractMethodError
          override def onGenerators(a: source.G) = f(generatorAsMorphism(a).asInstanceOf[f.source.M])
        }
      }
    }
  }
  override def internalize(t: net.metaphor.api.NaturalTransformationToSet) = {
    t match {
      case t: Datamap => t
      case _ => {
        require(t.sourceCategory == this)
        new Datamap {
          override val source = internalize(t.source)
          override val target = internalize(t.target)
          override def apply(o: Box) = t(o.asInstanceOf[t.sourceCategory.O])
        }
      }
    }
  }

  trait SpecializedFunctorsToSet extends super.SpecializedFunctorsToSet {
    override type O = ontology.F
    override type M = ontology.T
  }

  override val functorsToSet = Datasets
  sealed trait Datasets extends SpecializedFunctorsToSet

  // weird, moving the definition of this object up to the sealed trait causes a compiler crash.
  object Datasets extends Datasets

  def assertAcyclic: Ontology with Ontologies.Acyclic = new OntologyWrapper(this) with Ontologies.Acyclic
  def assertFree: Ontology with Ontologies.Free = new OntologyWrapper(this) with Ontologies.Free
  def assertFinite: Ontology with Ontologies.Finite = new OntologyWrapper(this) with Ontologies.Finite {
    def maximumWordLength(s: O, t: O) = ???
    def normalForm(m: M) = ???
  }
}

private class OntologyWrapper(val o: Ontology) extends Ontology {
  override type O = o.O
  override type G = o.G

  val minimumLevel = o.minimumLevel
  val maximumLevel = o.maximumLevel
  def objectsAtLevel(k: Int) = o.objectsAtLevel(k)
  def generators(s: O, t: O) = o.generators(s, t)
  def relations(s: O, t: O) = o.relations(s, t)
}

object Ontologies {
  trait Finite extends Ontology with net.metaphor.api.FiniteMorphisms { self =>
    // FIXME check that we're actually finite
  }

  trait Acyclic extends net.metaphor.api.Acyclic with Finite { self: Ontology =>
    override def assertAcyclic = this
    override def assertFree: Ontology with Ontologies.FreeAcyclic = new OntologyWrapper(this) with FreeAcyclic

  }
  trait Free extends net.metaphor.api.Free { self: Ontology =>
    override def assertAcyclic: Ontology with Ontologies.FreeAcyclic = new OntologyWrapper(this) with FreeAcyclic
    override def assertFree = this
  }
  trait FreeAcyclic extends net.metaphor.api.FreeAcyclic with Acyclic with Free { self: Ontology =>
    override def assertAcyclic = this
    override def assertFree = this
  }
}

