package net.metaphor.dsl
import net.metaphor.api.Ontology
import net.metaphor.api.Translation
import net.metaphor.api.Box
import net.metaphor.api.Arrow
import net.metaphor.api.Path
import net.metaphor.api.Dataset
import net.metaphor.api.Ontologies

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

    override val objects = boxes
    override def generators(source: Box, target: Box) = arrowMap(source, target).map(_.asPath)
    override def relations(source: Box, target: Box) = Nil // FIXME
  }

  def Ontology(objects: Traversable[String], arrows: Traversable[StringArrow], relations: Traversable[StringRelation] = Nil): Ontology = {
    // Construct a new ontology object
    new ConcreteOntology(objects, arrows, relations)
  }

  def Translation(source: Ontology, target: Ontology, onObjects: String => String, onMorphisms: StringArrow => StringPath): Translation = {
    val source_ = source
    val target_ = target

    val objectMap = (for (s <- source.objects) yield {
      val t = target.objects.find(_.name == onObjects(s.name)).get
      s -> t
    }).toMap
    val morphismMap = (for (
      p @ Path(sb, List(s)) <- source.allGenerators
    ) yield {
      s -> Path(source = objectMap(sb),
        arrows = for (StringArrow(ts, tp, to) <- onMorphisms(StringArrow(s.source.name, s.name, s.target.name)).arrows) yield {
          target.allGenerators.map(_.arrows.head).find(a => a.source.name == ts && a.name == tp && a.target.name == to).get
        })
    }).toMap

    // construct a new translation object
    new Translation {
      override val source = source_
      override val target = target_
      override def onObjects(o: Box) = objectMap(o)
      override def onMorphisms(m: Path) = Path(objectMap(m.source), m.arrows.map(morphismMap(_)).map(_.arrows).flatten)
    }
  }

  def Dataset(source: Ontology, onObjects: String => List[String], onMorphisms: StringArrow => (String => String)): source.Dataset = {

    val objectMap = (for (s <- source.objects) yield {
      s -> onObjects(s.name)
    }).toMap
    val morphismMap = (for (
      Path(sb, List(s)) <- source.allGenerators
    ) yield {
      s -> onMorphisms(StringArrow(s.source.name, s.name, s.target.name)).asInstanceOf[Any => Any]
    }).toMap

    new source.Dataset {
      override def onObjects(o: Box) = objectMap(o)
      override def onMorphisms(m: Path) = m.arrows.map(morphismMap(_)).foldLeft(identity[Any] _)(_.andThen(_))
    }
  }
}

