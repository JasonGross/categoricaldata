package net.metaphor.dsl
import net.metaphor.api.Arrow
import net.metaphor.api.Box
import net.metaphor.api.FiniteMorphisms
import net.metaphor.api.FiniteTarget
import net.metaphor.api.Ontology
import net.metaphor.api.Ontologies
import net.metaphor.api.Translation
import net.metaphor.api.FFunction

object Sentences {
  implicit def stringAsPath(s: String) = StringSource(s)

  trait StringPath {
    def source: String
    def arrows: List[StringArrow]
    def target: String

    def ===(other: StringPath) = StringRelation(this, other)
  }
  case class ConcreteStringPath(source: String, arrows: List[StringArrow]) extends StringPath {
    def ---(p: String) = IncompleteStringPath(this, p)
    def target = arrows.lastOption.map(_.target).getOrElse(source)
  }
  case class IncompleteStringPath(path: StringPath, p: String) {
    def -->(o: String) = ConcreteStringPath(path.source, path.arrows ::: List(StringArrow(path.target, p, o)))
  }
  case class StringArrow(source: String, label: String, target: String) extends StringPath {
    def arrows = List(this)
    def ---(p: String) = IncompleteStringPath(ConcreteStringPath(source, List(this)), p)
  }
  case class StringSource(source: String) extends StringPath {
    def arrows = Nil
    def target = source
    def ---(p: String) = IncompleteStringArrow(p)

    def identity = ConcreteStringPath(source, Nil)

    case class IncompleteStringArrow(p: String) {
      def -->(o: String) = StringArrow(source, p, o)
    }
  }

  case class StringRelation(lhs: StringPath, rhs: StringPath)

  class ConcreteOntology(_objects: Traversable[String], _arrows: Traversable[StringArrow], _relations: Traversable[StringRelation]) extends Ontology {
    val boxes = _objects.toList map { Box(_) }

    val arrowMap = (for (StringArrow(s, p, o) <- _arrows.toList) yield {
      val sb = boxes.find(_.name == s).get
      val ob = boxes.find(_.name == o).get
      Arrow(sb, ob, p)
    }).groupBy(a => (a.source, a.target)).withDefaultValue(Nil)

    val minimumLevel = 0
    val maximumLevel = 0
    def objectsAtLevel(k: Int) = if (k == 0) boxes else Nil
    override def generators(source: Box, target: Box) = arrowMap(source, target)
    override def relations(source: Box, target: Box) = Nil // FIXME parse relations from the DSL
  }

  def Ontology(objects: Traversable[String], arrows: Traversable[StringArrow], relations: Traversable[StringRelation] = Nil): Ontology = {
    // Construct a new ontology object
    new ConcreteOntology(objects, arrows, relations)
  }

  class ConcreteTranslation(override val source: Ontology, override val target: Ontology with Ontologies.Finite, onObjects: String => String, onMorphisms: StringArrow => StringPath) extends Translation {
    private val objectMap: Map[Box, Box] = (for (s <- source.objects) yield {
      val t = target.objects.find(_.name == onObjects(s.name)).get
      s -> t
    }).toMap
    private val morphismMap: Map[Arrow, target.M] = (for (
      a <- source.allGenerators
    ) yield {
      val morphisms = for (StringArrow(ts, tp, to) <- onMorphisms(StringArrow(a.source.name, a.name, a.target.name)).arrows) yield {
        target.generatorAsMorphism(target.allGenerators.find(a => a.source.name == ts && a.name == tp && a.target.name == to).get)
      }
      a -> target.compose(objectMap(source.generatorSource(a)), morphisms)
    }).toMap

    override def onObjects(o: Box) = objectMap(o)
    // And again, replacing source.G with the apparently equivalent Arrow causes an AbstractMethodError
    override def onGenerators(a: source.G) = morphismMap(a)
  }

  def Translation(source: Ontology, target: Ontology with Ontologies.Finite, onObjects: String => String, onMorphisms: StringArrow => StringPath): Translation with FiniteTarget = {
    // construct a new translation object
    new ConcreteTranslation(source, target, onObjects, onMorphisms) with FiniteTarget
  }

  def Dataset(source: Ontology, onObjects: String => List[String], onMorphisms: StringArrow => (String => String)): source.Dataset = {

    val objectMap = (for (s <- source.objects) yield {
      s -> onObjects(s.name)
    }).toMap
    val morphismMap = (for (
      a <- source.allGenerators
    ) yield {
      a -> onMorphisms(StringArrow(a.source.name, a.name, a.target.name)).asInstanceOf[Any => Any]
    }).toMap

    new source.Dataset {
      override def onObjects(o: source.O) = objectMap(o)
      // WEIRD: changing source.G to Arrow (which should be fine) results in AbstractMethodError at runtime. Compiler bug?
      override def onGenerators(a: source.G): FFunction = new FFunction(a) {
        override def toFunction = morphismMap(a)
      }
    }
  }
}

