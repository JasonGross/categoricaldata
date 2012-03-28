package net.categoricaldata.ontology
import net.categoricaldata.category._
import net.categoricaldata.sets._
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait Box {
  def name: String
  override def toString = "\"" + name + "\""
}

object Box {
  private case class i(name: String) extends Box
  def apply(name: String): Box = i(name)
}

case class Arrow(source: Box, target: Box, name: String) {
  override def toString = source.toString + " --- \"" + name + "\" --> " + target.toString
}

trait Ontology extends FinitelyPresentedCategory { ontology =>
  override type O = Box
  override type G = Arrow

  override def generatorSource(g: G) = g.source
  override def generatorTarget(g: G) = g.target

  override lazy val hashCode = toString.hashCode

  override def toString = {
    "Ontology(objects = " + (for (o <- objects) yield "\"" + o.name + "\"") + ", arrows = " + allGenerators + ", relations = " + allRelations.map(p => p._1 + " === " + p._2) + ")"
  }
  
  object identityTranslation extends Identity with Translation
  
  trait Dataset extends FunctorToSet with net.categoricaldata.ontology.Dataset { dataset =>
    abstract class DatasetFunction(g: G) extends net.categoricaldata.sets.FFunction {
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

    override def hashCode = toString.hashCode

    override lazy val toString = toStringHelper("Dataset")

    // TODO provide some way to let the user help out. 
    def findIsomorphismsTo(other: Ontology#Dataset): Iterable[Datamap] = {
      require(other.source == ontology)

      sealed trait cBox extends Box
      case class bBox(b: Box) extends cBox {
        def name = b.name
      }
      case class aBox(a: Arrow) extends cBox {
        def name = a.toString
      }

      val compositionDiagram = new Ontology with Ontology.FreeAcyclic {
        override val minimumLevel = 0
        override val maximumLevel = 1
        override def objectsAtLevel(k: Int) = {
          k match {
            case 0 => {
              ontology.objects.map { box => bBox(box) }
            }
            case 1 => {
              ontology.allGenerators.map { arrow => aBox(arrow) }
            }
            case _ => Nil
          }
        }
        def generators(source: Box, target: Box) = {
          source match {
            case bBox(box) => {
              target match {
                case aBox(arrow) => {
                  (if (arrow.source == box) List(Arrow(source, target, "left")) else Nil) ::: (if (arrow.target == box) List(Arrow(source, target, "right")) else Nil)
                }
                case bBox(_) => Nil
              }
            }
            case aBox(_) => Nil // no arrows out of arrows!
          }
        }
        def relations(source: Box, target: Box) = Nil
      }

      val noninvariantBijections = (new compositionDiagram.Dataset {
        def onObjects(o: Box) = {
          o match {
            case bBox(box) => {
              Sets.bijections(dataset(box), other(box))
            }
            case aBox(Arrow(a, b, _)) => {
              Sets.functions(dataset(a), other(b))
            }
          }
        }
        def onGenerators(g: Arrow) = {
          g match {
            case Arrow(s, t, direction) => {
              (t, direction) match {
                case (aBox(arrow), "left") => {
                  new DatasetFunction(g) {
                    def toFunction = { f => f.asInstanceOf[FFunction] andThen other.onGenerators(arrow) }
                  }
                }
                case (aBox(arrow), "right") => {
                  new DatasetFunction(g) {
                    def toFunction = { f => dataset.onGenerators(arrow) andThen f.asInstanceOf[FFunction] }
                  }
                }
              }
            }
          }
        }
      }).memo

      for (bijection <- noninvariantBijections.limitSet.toIterable) yield {
        new Datamap {
          override val source = dataset
          override val target = Datasets.internalize(other)
          // ACHTUNG --- this relies on the inner implementation of limit; in particular that the set it produces is a set of FFunctions, each sending Boxes to FFunctions
          override def apply(o: Box) = bijection.asInstanceOf[FFunction].toFunction.asInstanceOf[Box => FFunction](o)
        }
      }
    }

    def isIsomorphicTo(other: Ontology#Dataset) = findIsomorphismsTo(other).nonEmpty

    lazy val grothendieck: Translation = {
        case class GrothendieckBox(box: Box, element: Any) extends Box {
          def name = "(" + box.name + ": " + element.toString + ")"
        }
      
      object grothendieckOntology extends Ontology {
        override def objectsAtLevel(k: Int) = {
          for (o <- dataset.source.objectsAtLevel(k); x <- dataset(o).toIterable) yield {
            GrothendieckBox(o, x)
          }
        }
        override val minimumLevel = dataset.source.minimumLevel
        override val maximumLevel = dataset.source.maximumLevel
        override def generators(s: Box, t: Box) = {
          (s, t) match {
            case (GrothendieckBox(so, sx), GrothendieckBox(to, tx)) => {
              for (g <- dataset.source.generators(so, to); if dataset.onGenerators(g).toFunction(sx) == tx) yield Arrow(s, t, g.name)
            }
          }
        }
        override def relations(s: Box, t: Box) = {
          ??? // MATH what are the relations in the grothendieck construction
        }
        override def pathEquality(p1: Path, p2: Path) = ???
      }

      new Translation {
        override val source = grothendieckOntology
        override val target: ontology.type = ontology
        
        override def onObjects(o: Box) = o match {
          case GrothendieckBox(box, element) => box
        }
        override def onGenerators(g: Arrow) = g match {
          case Arrow(GrothendieckBox(so, _), GrothendieckBox(to, _), name) => ontology.generatorAsMorphism(Arrow(so, to, name))
        }
      }
    }

    class DatasetMemo extends Dataset {
      import net.tqft.toolkit.functions.Memo
      val memoOnObjects = Memo(dataset.onObjects _)
      val memoOnGenerators = Memo(dataset.onGenerators _)
      override def onObjects(o: O) = memoOnObjects(o)
      override def onGenerators(g: G) = memoOnGenerators(g)
    }

    lazy val memo: ontology.Dataset = new DatasetMemo

    def toJSON = net.categoricaldata.server.json.Pack.packDataset(this)
  }
  trait Datamap extends NaturalTransformationToSet with net.categoricaldata.ontology.Datamap { datamap =>
    override def toString = "Datamap(\n  onObjects = Map(\n" + (for (o <- ontology.objects) yield "    " + o.toString + " -> " + datamap(o).toString).mkString(",\n    ") + "))"
  }

  //  override type F = Dataset
  //  override type T = Datamap

  trait FunctorsToSet extends super.FunctorsToSet {
    override type O = Dataset
    override type M = Datamap

    override def internalize(f: net.categoricaldata.category.FunctorToSet) = {
      f match {
        case f: Dataset => f
        case _ => {
          require(f.source == ontology)
          new Dataset {
            override def onObjects(o: Box) = f(o.asInstanceOf[f.source.O])
            // and yet another weird one: replacing this with Arrow causes an AbstractMethodError
            override def onGenerators(a: source.G) = f(generatorAsMorphism(a).asInstanceOf[f.source.M])
          }
        }
      }
    }

    override def internalize(t: net.categoricaldata.category.NaturalTransformationToSet) = {
      t match {
        case t: Datamap => t
        case _ => {
          require(t.sourceCategory == ontology)
          new Datamap {
            override val source = internalize(t.source)
            override val target = internalize(t.target)
            override def apply(o: Box) = t(o.asInstanceOf[t.sourceCategory.O])
          }
        }
      }
    }
  }

  override type D = Datasets

  override val functorsToSet = Datasets
  sealed trait Datasets extends FunctorsToSet

  object Datasets extends Datasets

  class OppositeOntology extends Ontology with OppositeFinitelyPresentedCategory { opposite =>
    override type G = Arrow
    override def reverseGenerator(g: Arrow) = Arrow(g.target, g.source, "(" + g.name + ")^op")
    val unop = """\((.*)\)\^op""".r
    override def unreverseGenerator(g: Arrow) = Arrow(g.target, g.source, g.name match { case unop(name) => name; case _ => throw new NullPointerException })
  }

  override lazy val opposite: OppositeOntology = new OppositeOntology {}

  def assertAcyclic: Ontology with Ontology.Acyclic = {
    this match {
      case o: Ontology.Acyclic => o
      case _ => (new OntologyWrapper with Ontology.Acyclic with FinitelyPresentedCategory.FiniteByExhaustion).verifyAcyclic
    }
  }
  def assertFree: Ontology with Ontology.Free = {
    this match {
      case o: Ontology.Free => o
      case _ =>
        (new OntologyWrapper with Ontology.Free).verifyFree
    }
  }
  def assertFinite: Ontology with Ontology.Finite = {
    this match {
      case o: Ontology.Finite => o
      case _ => new OntologyWrapper with Ontology.Finite with FinitelyPresentedCategory.FiniteByExhaustion
    }
  }

  trait OntologyWrapper extends super.Wrapper with Ontology

  class FullSubcategory(spannedBy: List[O]) extends super.FullSubcategory(spannedBy) with Ontology

  class FullSubcategoryInclusion(spannedBy: List[O]) extends super.FullSubcategoryInclusion(spannedBy) with Translation {
    override val source = new FullSubcategory(spannedBy)
  }

  override def fullSubcategoryInclusion(spannedBy: List[O]): FullSubcategoryInclusion = new FullSubcategoryInclusion(spannedBy)
  override def fullSubcategory(spannedBy: List[O]): FullSubcategory = fullSubcategoryInclusion(spannedBy).source

  def fullSubcategoryInclusion(spannedBy: String*)(implicit d: DummyImplicit): FullSubcategoryInclusion = fullSubcategoryInclusion(spannedBy.toList.map(Box(_)))
  def fullSubcategory(spannedBy: String*)(implicit d: DummyImplicit): FullSubcategory = fullSubcategoryInclusion(spannedBy: _*).source

  def findAllTranslationsTo(other: Ontology): Iterable[Translation] = {
    trait TranslationToOther extends Translation {
      override val target: other.type = other
      override val source: ontology.type = ontology
    }

    // FIXME make this work for anything other than the terminal ontology.
    objects match {
      case e :: Nil => {
        generators(e, e) match {
          case Nil => {
            for (x <- other.objects) yield new TranslationToOther {
              override def onObjects(o: Box) = x
              override def onGenerators(g: Arrow) = throw new IllegalArgumentException
            }
          }
          case _ => ???
        }
      }
      case _ => ???
    }
  }

  def toJSON = net.categoricaldata.server.json.Pack.packOntology(this)

  trait PartialDataset extends FinitelyGeneratedCategoryOver with Translation { partialDataset =>
    def complete_? : Boolean = ???
    def valid_? : Boolean = ???
    def wellDefined_? : Boolean = ???

    def toDataset: ontology.Dataset = new ontology.Dataset {
      val boxMap = partialDataset.source.objects.groupBy(partialDataset.onObjects(_))
      val arrowMap = partialDataset.source.allGenerators.groupBy(partialDataset.onGenerators(_))

      override def onObjects(o: Box) = boxMap(o).map(_.name)
      override def onGenerators(g: Arrow) = FFunction(onObjects(g.source), onObjects(g.target), arrowMap(generatorAsMorphism(g)).map({ case Arrow(s, t, _) => s.name -> t.name }).toMap)
    }
  }
}

object Ontology {
  trait Finite extends Ontology with FinitelyPresentedCategory.FiniteMorphisms

  trait Acyclic extends FinitelyPresentedCategory.Acyclic with Finite { ontology: Ontology =>
    override def assertAcyclic = this
    override def assertFree: Ontology with Ontology.FreeAcyclic = (new ontology.OntologyWrapper with FreeAcyclic).verifyFree

  }
  trait Free extends FinitelyPresentedCategory.Free { ontology: Ontology =>
    override def assertAcyclic: Ontology with Ontology.FreeAcyclic = (new ontology.OntologyWrapper with FreeAcyclic).verifyAcyclic
    override def assertFree = this
  }
  trait FreeAcyclic extends FinitelyPresentedCategory.FreeAcyclic with Acyclic with Free { ontology: Ontology =>
    override def assertAcyclic = this
    override def assertFree = this
  }

  import net.categoricaldata.dsl.Sentences

  def construct(boxes: List[Box], arrows: List[Arrow], relations: List[(Path[Box, Arrow], Path[Box, Arrow])] = Nil, json: Option[String] = None): Ontology = {
    class ConcreteOntology(_boxes: List[Box], _arrows: List[Arrow], _relations: List[(net.categoricaldata.category.Path[Box, Arrow], net.categoricaldata.category.Path[Box, Arrow])], _json: Option[String]) extends Ontology {
      private val arrowMap = _arrows.groupBy(a => (a.source, a.target)).withDefaultValue(Nil)

      private val relationsMap = _relations.groupBy(a => (a._1.source, a._1.target)).withDefaultValue(Nil)

      val minimumLevel = 0
      val maximumLevel = 0
      def objectsAtLevel(k: Int) = if (k == 0) boxes else Nil
      override def generators(source: Box, target: Box) = arrowMap(source, target)
      override def relations(source: Box, target: Box) = relationsMap(source, target)

      override def pathEquality(p1: Path, p2: Path) = ???

      override def toJSON = super.toJSON.copy(json = _json)
    }
    // Construct a new ontology object
    new ConcreteOntology(boxes, arrows, relations, json)
  }
  def apply(objects: Traversable[String], arrows: Traversable[Sentences.StringArrow], relations: Traversable[Sentences.StringRelation] = Nil, json: Option[String] = None): Ontology = {
    val allBoxes = objects.toList map { Box(_) }

    def stringArrowToArrow(sa: Sentences.StringArrow) = Arrow(stringToBox(sa.source), stringToBox(sa.target), sa.label)
    def stringToBox(s: String) = allBoxes.find(_.name == s).get

    val allArrows: List[Arrow] = for (sa <- arrows.toList) yield stringArrowToArrow(sa)

    val allRelations: List[(Path[Box, Arrow], Path[Box, Arrow])] = (for (Sentences.StringRelation(lhs, rhs) <- relations.toList) yield {
      val source: Box = stringToBox(lhs.source)
      val target: Box = stringToBox(lhs.target)
      val leftMorphisms = lhs.arrows.map(stringArrowToArrow(_))
      val rightMorphisms = rhs.arrows.map(stringArrowToArrow(_))
      (Path(source, target, leftMorphisms), Path(source, target, rightMorphisms))
    })

    construct(allBoxes, allArrows, allRelations, json)
  }

}

