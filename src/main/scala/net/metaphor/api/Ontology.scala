package net.metaphor.api

case class Box(name: String) {
  def identity = Path(this, Nil)
}

case class Arrow(source: Box, target: Box, name: String) {
  def asPath = Path(source, List(this))
}

case class Path(source: Box, arrows: List[Arrow]) {
  def target = arrows.last.target

  override def toString = source.name + (for (a <- arrows) yield " --- " + a.name + " ---> " + a.target).mkString
}

trait Ontology extends FinitelyPresentedCategory[Box, Path, Ontology] { ontology =>
  override def compose(m1: Path, m2: Path) = {
    require(m2.source == m1.target)
    Path(m1.source, m1.arrows ::: m2.arrows)
  }
  override def source(m: Path) = m.source
  override def target(m: Path) = m.target
  override def identity(o: Box) = o.identity

  def opposite = new Ontology with Opposite

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

  override lazy val adjoinTerminalObject: WithTerminalObject = new Ontology with WithTerminalObject { ato =>
    val terminalObject = Box("*")
    def morphismFrom(o: Box) = Arrow(o, terminalObject, "*").asPath

    val minimumLevel = ontology.minimumLevel
    val maximumLevel = ontology.maximumLevel + 1
    def objectsAtLevel(k: Int) = if (k == maximumLevel) {
      List(terminalObject)
    } else {
      ontology.objectsAtLevel(k)
    }
    def generators(source: Box, target: Box) = {
      if (target == terminalObject) {
        List(morphismFrom(source))
      } else {
        ontology.generators(source, target)
      }
    }
    def relations(source: Box, target: Box) = {
      if (target == terminalObject) {
        ???
      } else {
        ontology.relations(source, target)
      }
    }
  }
  override lazy val adjoinInitialObject: WithInitialObject = new Ontology with WithInitialObject { aio =>
    val initialObject = Box(".")
    def morphismTo(o: Box) = Arrow(initialObject, o, "*").asPath

    val minimumLevel = ontology.minimumLevel - 1
    val maximumLevel = ontology.maximumLevel
    def objectsAtLevel(k: Int) = if (k == minimumLevel) {
      List(initialObject)
    } else {
      ontology.objectsAtLevel(k)
    }
    def generators(source: Box, target: Box) = {
      if (source == initialObject) {
        List(morphismTo(target))
      } else {
        ontology.generators(source, target)
      }
    }
    def relations(source: Box, target: Box) = {
      if (source == initialObject) {
        ???
      } else {
        ontology.relations(source, target)
      }
    }
  }

  trait Dataset extends FunctorToSet with net.metaphor.api.Dataset
  trait Datamap extends NaturalTransformationToSet[Dataset] with net.metaphor.api.Datamap[Dataset]

  object Dataset {
    // yoink an arbitrary Dataset into one living on this Ontology
    def apply(dataset: Ontology#Dataset): Dataset = {
      require(dataset.source == ontology)
      new Dataset {
        override def onObjects(o: Box) = dataset.onObjects(o)
        override def onMorphisms(m: Path) = dataset.onMorphisms(m)
      }
    }
  }
  object Datamap {
    def apply(datamap: Ontology#Datamap): Datamap = {
      require(datamap.sourceCategory == ontology)
      new Datamap {
        def source = Dataset(datamap.source)
        def target = Dataset(datamap.target)
        def apply(o: Box) = datamap(o)
      }
    }
  }

  override type F = Dataset
  override type T = Datamap
  override type CSets = Datasets

  override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[Box, Path, Ontology]): Dataset = {
    f match {
      case f: Dataset => f
      case _ => {
        require(f.source == ontology)
        new Dataset {
          def onObjects(o: Box) = f(o)
          def onMorphisms(m: Path) = f(m)
        }
      }
    }
  }
  override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[Box, Path, Ontology, Dataset]): Datamap = {
    t match {
      case t: Datamap => t
      case _ => {
        require(t.sourceCategory == ontology)
        new Datamap {
          val source = liftFunctorToSet(t.source)
          val target = liftFunctorToSet(t.target)
          def apply(o: Box) = t(o)
        }
      }
    }
  }

  override val functorsToSet = Datasets
  sealed trait Datasets extends FunctorsToSet

  // weird, moving the definition of this object up to the sealed trait causes a compiler crash.
  object Datasets extends Datasets {
    override def lift(t: HeteroNaturalTransformation[Box, Path, Ontology, Set, Function, Sets, Dataset]) = new Datamap {
      def source = t.source
      def target = t.target
      def apply(o: Box) = t(o)
    }
  }

  def assertAcyclic: Ontology with Ontologies.Acyclic = new OntologyWrapper(this) with Ontologies.Acyclic
  def assertGraph: Ontology with Ontologies.Graph = new OntologyWrapper(this) with Ontologies.Graph
}

private class OntologyWrapper(o: Ontology) extends Ontology {
  val minimumLevel = o.minimumLevel
  val maximumLevel = o.maximumLevel
  def objectsAtLevel(k: Int) = o.objectsAtLevel(k)
  def generators(s: Box, t: Box) = o.generators(s, t)
  def relations(s: Box, t: Box) = o.relations(s, t)
}

object Ontologies extends FinitelyPresentedCategories[Box, Path, Ontology] {
  trait Finite extends net.metaphor.api.FiniteMorphisms[Box, Path, Ontology] with Ontology { self =>
    override type CO = CategoryOver[Box, Path, Ontology, FunctorTo[Box, Path, Ontology]]
    override type FO = FunctorOver[Box, Path, Ontology, Functor[Box, Path, Ontology], FunctorTo[Box, Path, Ontology], CO]
    override type CsO = CategoriesOver

    sealed trait CategoriesOver extends super.CategoriesOver[Box, Path, Ontology, Functor[Box, Path, Ontology], FunctorTo[Box, Path, Ontology], CO, FO, CategoriesOver]
    object CategoriesOver extends CategoriesOver {
      override def lift(_source: CO, _target: CO, f: Functor[Box, Path, Ontology]) = new FO {
        def source = _source
        def target = _target
        def functor = f
      }
    }
    override val categoriesOver = CategoriesOver
  }

  trait Acyclic extends net.metaphor.api.Acyclic[Box, Path, Ontology] with Finite { self: Ontology =>
    override def assertAcyclic = this
    override def assertGraph: Ontology with Ontologies.AcyclicGraph = new OntologyWrapper(this) with AcyclicGraph

  }
  trait Graph extends net.metaphor.api.Graph[Box, Path, Ontology] { self: Ontology =>
    override def assertAcyclic: Ontology with Ontologies.AcyclicGraph = new OntologyWrapper(this) with AcyclicGraph
    override def assertGraph = this
  }
  trait AcyclicGraph extends net.metaphor.api.AcyclicGraph[Box, Path, Ontology] with Acyclic with Graph { self: Ontology =>
    override def assertAcyclic = this
    override def assertGraph = this
  }
}

