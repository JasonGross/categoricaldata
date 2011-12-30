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
    abstract class FFunction(g: G) extends net.metaphor.api.FFunction {
      override def source = onObjects(generatorSource(g))
      override def target = onObjects(generatorTarget(g))
    }
    
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

    // TODO provide some way to let the user help out. 
    def findIsomorphismsTo(other: Ontology#Dataset): Iterable[Datamap] = {
      require(other.source == this.source)

      val compositionDiagram = new Ontology {
        // FIXME deal with objects at other levels
        val unbox: Map[Box, Either[Box, Arrow]] = {
          ((for (b <- ontology.objectsAtLevel(0)) yield b -> Left(b)) :::
            (for (a <- ontology.allGenerators) yield Box(a.toString) -> Right(a))).toMap
        }

        val minimumLevel = 0
        val maximumLevel = 0
        def objectsAtLevel(k: Int) = {
          k match {
            case 0 => {
              unbox.keys.toList
            }
            case _ => Nil
          }
        }
        def generators(source: Box, target: Box) = {
          unbox(source) match {
            case Left(a) => {
              unbox(target) match {
                case Right(arrow) => {
                  (if (arrow.source == a) List(Arrow(source, target, "left")) else Nil) ::: (if (arrow.target == a) List(Arrow(source, target, "right")) else Nil)
                }
                case Left(_) => Nil
              }
            }
            case Right(_) => Nil // no arrows out of arrows!
          }
        }
        def relations(source: Box, target: Box) = Nil
      }

      val noninvariantBijections = new compositionDiagram.Dataset {
        def onObjects(o: Box) = {
          compositionDiagram.unbox(o) match {
            case Left(box) => {
              Sets.bijections(dataset(box), other(box))
            }
            case Right(Arrow(a, b, _)) => {
              Sets.bijections(dataset(a), other(b))
            }
          }
        }
        def onGenerators(g: Arrow) = {
          g match {
            case Arrow(s, t, direction) => {
              (compositionDiagram.unbox(t), direction) match {
                case (Right(arrow), "left") => {
                  new FFunction(g) {
                    def toFunction = { f => f.asInstanceOf[FFunction] andThen other.onGenerators(g) }
                  }
                }
                case (Right(arrow), "right") => {
                  new FFunction(g) {
                    def toFunction = { f => dataset.onGenerators(g) andThen f.asInstanceOf[FFunction] }
                  }
                }
              }
            }
          }
        }
      }

      for(bijection <- noninvariantBijections.limitSet.toIterable) yield {
        new Datamap {
          override val source = dataset
          override val target = internalize(other)
          // ACHTUNG --- this relies on the inner implementation of colimit; in particular that the set it produces is a set of (Box => FFunction)
          override def apply(o: Box) = bijection.asInstanceOf[Box => FFunction](o)
        }
      }
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

  class OppositeOntology extends Ontology with OppositeFinitelyPresentedCategory { opposite =>
    override type G = Arrow
    override def reverse(g: Arrow) = Arrow(g.target, g.source, "(" + g.name + ")^op")
    val unop = """\((.*)\)\^op""".r
    override def unreverse(g: Arrow) = Arrow(g.target, g.source, g.name match { case unop(name) => name; case _ => throw new NullPointerException })
  }
  
  override lazy val opposite: OppositeOntology = new OppositeOntology { }
  
  def assertAcyclic: Ontology with Ontologies.Acyclic = {
    this match {
      case o: Ontologies.Acyclic => o
      case _ => new OntologyWrapper(this) with Ontologies.Acyclic
    }
  }
  def assertFree: Ontology with Ontologies.Free = {
    this match {
      case o: Ontologies.Free => o
      case _ =>
        new OntologyWrapper(this) with Ontologies.Free
    }
  }
  def assertFinite: Ontology with Ontologies.Finite = {
    this match {
      case o: Ontologies.Finite => o
      case _ => new OntologyWrapper(this) with Ontologies.Finite {
        def maximumWordLength(s: O, t: O) = ???
        def normalForm(m: M) = ???
      }
    }
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
  trait Finite extends Ontology with net.metaphor.api.FiniteMorphisms { 
    // FIXME check that we're actually finite
  }

  trait Acyclic extends net.metaphor.api.Acyclic with Finite { ontology: Ontology =>
    override def assertAcyclic = this
    override def assertFree: Ontology with Ontologies.FreeAcyclic = new OntologyWrapper(this) with FreeAcyclic

  }
  trait Free extends net.metaphor.api.Free { ontology: Ontology =>
    override def assertAcyclic: Ontology with Ontologies.FreeAcyclic = new OntologyWrapper(this) with FreeAcyclic
    override def assertFree = this
  }
  trait FreeAcyclic extends net.metaphor.api.FreeAcyclic with Acyclic with Free { ontology: Ontology =>
    override def assertAcyclic = this
    override def assertFree = this
  }
}

