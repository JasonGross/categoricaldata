package net.metaphor.dsl
import net.metaphor.api.Ontology
import net.metaphor.api.Translation
import net.metaphor.api.Box
import net.metaphor.api.Arrow
import net.metaphor.api.Path
import net.metaphor.api.Dataset

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
  
  def ontology(objects: Traversable[String], arrows: Traversable[StringArrow], relations: Traversable[StringRelation] = Nil): Ontology = {
    val boxes = objects.toList map { Box(_) }

    val arrowMap = (for (StringArrow(s, p, o) <- arrows.toList) yield {
      val sb = boxes.find(_.name == s).get
      val ob = boxes.find(_.name == o).get
      Arrow(sb, ob, p)
    }).groupBy(a => (a.source, a.target)).withDefaultValue(Nil)

    
    
    // Construct a new ontology object
    new Ontology {
      override val objects = boxes
      override def generators(source: Box, target: Box) = arrowMap(source, target).map(_.asPath)
      override def relations(source: Box, target: Box) = Nil // FIXME
    }
  }

  def functor(source: Ontology, target: Ontology, onObjects: String => String, onMorphisms: StringArrow => StringPath): Translation = {
    val source_ = source
    val target_ = target

    val objectMap = (for (s <- source.objects) yield {
      val t = target.objects.find(_.name == onObjects(s.name)).get
      s -> t
    }).toMap
    val morphismMap = (for (
      p @ Path(sb, List(s)) <- source.allGenerators
    ) yield {
      s -> Path(start = objectMap(sb),
        arrows = for (StringArrow(ts, tp, to) <- onMorphisms(StringArrow(s.source.name, s.name, s.target.name)).arrows) yield {
          target.allGenerators.map(_.arrows.head).find(a => a.source.name == ts && a.name == tp && a.target.name == to).get
        })
    }).toMap

    // construct a new translation object
    new Translation {
      override def source = source_
      override def target = target_
      override def onObjects(o: Box) = objectMap(o)
      override def onMorphisms(m: Path) = Path(objectMap(m.start), m.arrows.map(morphismMap(_)).map(_.arrows).flatten)
    }
  }

  def dataset(source: Ontology, onObjects: String => List[String], onMorphisms: StringArrow => (String => String)): source.Dataset = {
    
    val objectMap = (for(s <- source.objects) yield {
      s -> net.metaphor.api.Set(onObjects(s.name))
    }).toMap
    val morphismMap = (for(
      Path(sb, List(s)) <- source.allGenerators
    ) yield {
      s -> net.metaphor.api.Function(onMorphisms(StringArrow(s.source.name, s.name, s.target.name)))
    }
      ).toMap
    
    new source.Dataset {
      override def onObjects(o: Box) = objectMap(o)
      override def onMorphisms(m: Path) = m.arrows.map(morphismMap(_)).reduce(_.andThen(_))
    }
  }
}

