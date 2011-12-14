package net.metaphor.dsl
import net.metaphor.api.Ontology
import net.metaphor.api.Translation
import net.metaphor.api.Dataset
import net.metaphor.api.Box
import net.metaphor.api.Arrow
import net.metaphor.api.Path

object Sentences {
  implicit def stringAsArrowSource(s: String) = new StringArrowSource(s)
  implicit def arrowAsPath(a: StringArrow) = new StringPath(List(a))

  case class StringArrowSource(s: String) {
    def ---(p: String) = new PartialStringArrow(p)
    class PartialStringArrow(p: String) {
      def -->(o: String) = new StringArrow(s, p, o)
    }
  }

  case class StringArrow(s: String, p: String, o: String) {
    println(s + " " + p + " " + o)
    def ---(p: String) = new IncompleteStringPath(new StringPath(List(this)), p)
  }

  case class IncompleteStringPath(path: StringPath, p: String) {
    def --->(o: String) = new StringPath(path.arrows ::: List(new StringArrow(path.arrows.last.o, p, o)))
  }

  case class StringPath(arrows: List[StringArrow]) {
    def ---(p: String) = new IncompleteStringPath(this, p)
  }

  def category(objects: List[String], arrows: List[StringArrow]): Ontology = {
    val boxes = objects map { Box(_) }

    val arrowMap = (for (StringArrow(s, p, o) <- arrows) yield {
      val sb = boxes.find(_.name == s).get
      val ob = boxes.find(_.name == o).get
      Arrow(sb, ob, p)
    }).groupBy(_.source).mapValues(_.groupBy(_.target))

    // Construct a new Ontology object
    new Ontology {
      override val objects = boxes
      override def generators(source: Box, target: Box) = arrowMap(source)(target).map(_.asPath)
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
      p @ Path(sb, List(s)) <- source.generators
    ) yield {
      s -> Path(start = objectMap(sb),
        arrows = for (StringArrow(ts, tp, to) <- onMorphisms(StringArrow(s.source.name, s.name, s.target.name)).arrows) yield {
          target.generators.map(_.arrows.head).find(a => a.source.name == ts && a.name == tp && a.target.name == to).get
        })
    }).toMap

    new Translation {
      override def source = source_
      override def target = target_
      override def onObjects(o: Box) = objectMap(o)
      override def onMorphisms(m: Path) = Path(objectMap(m.start), m.arrows.map(morphismMap(_)).map(_.arrows).flatten)
    }
  }

  def dataset(source: Ontology, onObjects: String => List[String], onMorphisms: StringArrow => (String => String)): Dataset = ???
}

