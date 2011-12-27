package net.metaphor.api

case class Box(name: String)

case class Arrow(source: Box, target: Box, name: String)

trait Ontology extends FinitelyPresentedCategory[Ontology] { ontology =>
  override type O = Box
  override type G = Arrow

  override def generatorSource(g: G) = g.source
  override def generatorTarget(g: G) = g.target

  //  def opposite = new Ontology with Opposite

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

  trait Dataset extends FunctorToSet {
    override def equals(other: Any): Boolean = {
      other match {
        case other: Ontology#Dataset => {
          if (source != other.source) return false
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
  trait Datamap extends NaturalTransformationToSet[Dataset]
  
  override type F = Dataset
  override type T = Datamap
  override type CSets = Datasets

  override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[Ontology]): Dataset = {
    f match {
      case f: Dataset => f
      case _ => {
        require(f.source == ontology)
        new Dataset {
          override def onObjects(o: O) = f(o)
          override def onGenerators(g: G) = f(f.source.generatorAsMorphism(g))
        }
      }
    }
  }
  override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[Ontology, Dataset]): Datamap = {
    t match {
      case t: Datamap => t
      case _ => {
        require(t.sourceCategory == ontology)
        new Datamap {
          override val source = liftFunctorToSet(t.source)
          override val target = liftFunctorToSet(t.target)
          override def apply(o: Box) = t(o)
        }
      }
    }
  }

  override val functorsToSet = Datasets
  sealed trait Datasets extends FunctorsToSet

  // weird, moving the definition of this object up to the sealed trait causes a compiler crash.
  object Datasets extends Datasets {
    override def lift(t: HeteroNaturalTransformation[Ontology, Sets, Dataset]) = new Datamap {
      val source = t.source
      val target = t.target
      def apply(o: Box) = t(o)
    }
  }

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

object Ontologies extends FinitelyPresentedCategories[Ontology] {
  trait Finite extends Ontology with net.metaphor.api.FiniteMorphisms[Ontology] { self =>
    // FIXME check that we're actually finite
  }

  trait Acyclic extends net.metaphor.api.Acyclic[Ontology] with Finite { self: Ontology =>
    override def assertAcyclic = this
    override def assertFree: Ontology with Ontologies.FreeAcyclic = new OntologyWrapper(this) with FreeAcyclic

  }
  trait Free extends net.metaphor.api.Free[Ontology] { self: Ontology =>
    override def assertAcyclic: Ontology with Ontologies.FreeAcyclic = new OntologyWrapper(this) with FreeAcyclic
    override def assertFree = this
  }
  trait FreeAcyclic extends net.metaphor.api.FreeAcyclic[Ontology] with Acyclic with Free { self: Ontology =>
    override def assertAcyclic = this
    override def assertFree = this
  }
}

