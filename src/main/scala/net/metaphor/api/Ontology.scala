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

  override def toString = {
    "Dataset(\n" +
      "  source = " + source + ", \n" +
      "  onObjects = " + (for (o <- source.objects) yield o -> this(o).toIterable.toList).toMap + ", \n" +
      "  onMorphisms = Map(\n" + (for (g <- source.allGenerators; g1 = this(g).toFunction) yield "    " + g.toString + " -> " + (g + (for (x <- this(g.source).toIterable) yield x -> g1(x)).toMap.toString)).mkString("\n") + "  )\n)"
  }
}
trait Datamap[F <: Dataset] extends NaturalTransformationToSet[Box, Path, Ontology, F]

trait Ontology extends FinitelyPresentedCategory[Box, Path, Ontology] { ontology =>
  override def compose(m1: Path, m2: Path) = {
    require(m2.target == m1.source)
    Path(m1.source, m1.arrows ::: m2.arrows)
  }
  override def source(m: Path) = m.source
  override def target(m: Path) = m.target
  override def identity(o: Box) = o.identity

  override def equals(other: Any) = {
    other match {
      case other: Ontology => {
        objects == other.objects && allGenerators == other.allGenerators && allRelations == other.allRelations
      }
      case _ => false
    }
  }

  override def toString = {
    // TODO relations
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
    def morphismFrom(o: Box) = ???

    def objects = terminalObject :: ontology.objects
    def generators(source: Box, target: Box) = {
      if (target == terminalObject) {
        List(Arrow(source, target, "*").asPath)
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

  type F = Dataset
  type T = Datamap
  override type CSets = Datasets

  override val functorsToSet = Datasets
  sealed trait Datasets extends FunctorsToSet[Dataset, Datamap, Datasets]
  object Datasets extends Datasets {
    override def lift(t: HeteroNaturalTransformation[Box, Path, Ontology, Set, Function, Sets, Dataset]) = new Datamap {
      def source = t.source
      def target = t.target
      def apply(o: Box) = t(o)
    }
  }
}

// TODO implement InitialOntology similarly
object TerminalOntology extends TerminalFinitelyPresentedCategory[Box, Path, Ontology] with Ontology {
  val terminalObject = Box("*")
  def morphismFrom(o: Box) = terminalObject.identity
}

//object Ontologies extends FinitelyPresentedCategories[Box, Path, Ontology]

trait Translation extends Functor[Box, Path, Ontology] { translation =>
  val source: Ontology
  val target: Ontology

  trait ContravariantDataFunctor extends HeteroFunctor[target.Dataset, target.Datamap, target.Datasets, source.Dataset, source.Datamap, source.Datasets] {
    val source = translation.target.Datasets
    val target = translation.source.Datasets
    def apply(i: Ontology#Dataset) = {
      super.apply(translation.target.Dataset(i))
    }
    def apply(m: Ontology#Datamap) = {
      super.apply(translation.target.Datamap(m))
    }
  }
  trait CovariantDataFunctor extends HeteroFunctor[source.Dataset, source.Datamap, source.Datasets, target.Dataset, target.Datamap, target.Datasets] {
    val source = translation.source.Datasets
    val target = translation.target.Datasets
    def apply(i: Ontology#Dataset) = {
      super.apply(translation.source.Dataset(i))
    }
    def apply(m: Ontology#Datamap) = {
      super.apply(translation.source.Datamap(m))
    }
  }

  trait Pullback extends ContravariantDataFunctor
  trait Pushforward extends CovariantDataFunctor
  trait Shriek extends CovariantDataFunctor

  def pullback: Pullback = new Pullback {
    def onObjects(i: translation.target.Dataset) = new translation.source.Dataset {
      def onObjects(o: Box) = i(translation(o))
      def onMorphisms(m: Path) = i(translation(m))
    }
    def onMorphisms(m: translation.target.Datamap) = new translation.source.Datamap {
      def source = onObjects(m.source)
      def target = onObjects(m.target)
      def apply(o: Box) = m(translation(o))
    }
  }
  def pushforward: Pushforward = ???
  def shriek: Shriek = ???

  def ^* = pullback
  def __! = shriek
  def __* = pushforward
}
