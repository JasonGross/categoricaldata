package net.metaphor.api

case class Box(name: String) {
}

case class Arrow(source: Box, target: Box, name: String) {
}

//case class Path(source: Box, arrows: List[Arrow]) {
//  def target = arrows.lastOption.map(_.target).getOrElse(source)
//
//  override def toString = source.name + (for (a <- arrows) yield " --- " + a.name + " --> " + a.target).mkString
//}

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

  /* TODO (David) toString needs to *return* a string, not print one.
     */

  /*
     *  Here you're actually "overriding" an existing method (every Scala object comes with a predefined toString method,
     *  but by default it isn't very useful). Whenever you override, it's good practice to write "override def" instead of just "def".
     *  This tells the compiler to generate an error if, in fact, you're *not* overriding something.
     *  
     *  Moreover, once a method has the "override" keyword, anyone who later overrides it has to explicitly mark their method with "override".
     *  The default "toString" already has the "override" keyword, so it's actually necessary here.
     *  
     *  I've added it for you, but try removing it to see the compile error.
     */

  //    override def toString() = {
  //      println("Ontology")
  //      println("Objects: ")
  //      for (i <- objects) println(i)
  //      println(); println("Arrows: ")
  //      /*
  //       * TODO (David) I commented the next line  out, as it wasn't compiling. You're looking for "allGenerators", rather than arrows. Remind me
  //       * to give you a quick lesson on working out what methods are defined on which classes.  ---S
  //       */
  //      //      for (i <- arrows) println(i)
  //
  //      /*
  //       *  Finally, I'm adding the next line (which just generates an exception, if the program ever gets to here, because
  //       *  the toString method must return a String, and the compiler knows this and complains if it can't see a return value being generated.
  //       */
  //      ???
  //    }

  override lazy val adjoinTerminalObject: TerminalObjectAdjoined = new Ontology with TerminalObjectAdjoined {
    val terminalObject = Box("*")
    def generatorFrom(o: O) = Arrow(o, terminalObject, "*")

  }
  override lazy val adjoinInitialObject: InitialObjectAdjoined = new Ontology with InitialObjectAdjoined {
    val initialObject = Box(".")
    def generatorTo(o: O) = Arrow(initialObject, o, ".")
  }

  trait Dataset extends FunctorToSet with net.metaphor.api.Dataset {
    override def equals(other: Any): Boolean = {
      other match {
        case other: Dataset => {
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
        "  onMorphisms = Map(\n" + (for (g <- source.allGenerators; m = source.generatorAsMorphism(g); g1 = this(m).toFunction) yield "    " + m.toString + " -> " + (m + (for (x <- this(source.source(m)).toIterable) yield x -> g1(x)).toMap.toString)).mkString("\n") + "  )\n)"
    }

    // TODO define this recursively, and provide some way to let the user help out. 
    def findIsomorphismsTo(other: Ontology#Dataset): Iterable[Datamap] = ???
    def isIsomorphicTo(other: Dataset) = findIsomorphismsTo(other).nonEmpty

  }
  trait Datamap extends NaturalTransformationToSet[Dataset] with net.metaphor.api.Datamap[Dataset]

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
          override def onGenerators(g: G) = f(g.asInstanceOf[f.source.M])
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
  //TODO: Change assertGraph to assertFree
  def assertAcyclic: Ontology with Ontologies.Acyclic = new OntologyWrapper(this) with Ontologies.Acyclic
  def assertGraph: Ontology with Ontologies.Graph = new OntologyWrapper(this) with Ontologies.Graph
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
  trait Finite extends  Ontology with net.metaphor.api.FiniteMorphisms[Ontology] { self =>
    // FIXME check that we're actually finite
    
   
  }

  trait Acyclic extends net.metaphor.api.Acyclic[Ontology] with Finite { self: Ontology =>
    override def assertAcyclic = this
    override def assertGraph: Ontology with Ontologies.AcyclicGraph = new OntologyWrapper(this) with AcyclicGraph

  }
  trait Graph extends net.metaphor.api.Graph[Ontology] { self: Ontology =>
    override def assertAcyclic: Ontology with Ontologies.AcyclicGraph = new OntologyWrapper(this) with AcyclicGraph
    override def assertGraph = this
  }
  trait AcyclicGraph extends net.metaphor.api.AcyclicGraph[Ontology] with Acyclic with Graph { self: Ontology =>
    override def assertAcyclic = this
    override def assertGraph = this
  }
}

