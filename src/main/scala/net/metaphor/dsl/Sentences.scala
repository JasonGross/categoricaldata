package net.metaphor.dsl
import net.metaphor.api.Ontology
import net.metaphor.api.Translation
import net.metaphor.api.Dataset

object Sentences {
  implicit def stringAsArrowSource(s: String) = new ArrowSource(s)

  class ArrowSource(s: String) {
    def ---(p: String) = new PartialArrow(p)
    class PartialArrow(p: String) {
      def -->(o: String) = new Arrow(s, p, o)
    }
  }

  class Arrow(s: String, p: String, o: String) {
    println(s + " " + p + " " + o)
  }

  // FIXME
  type Path = Arrow

  def model(objects: List[String], arrows: List[Arrow]): Ontology = ???

  case class ObjectMap(source: String, target: String)
  case class MorphismMap(source: Arrow, target: Path)
  case class ObjectData(source: String, data: List[String])
  case class MorphismData(source: Arrow, data: Map[String, String])

  object to {
    def apply(t: String) = t
    def apply(t: Arrow) = t
    def apply(d: String*) = d.toList
    def apply(d: (String, String)*) = d.toMap
  }

  implicit def stringAsObjectSource(s: String) = new ObjectSource(s)
  class ObjectSource(s: String) {
    def maps(t: String) = mapsto(t)
    def mapsto(t: String) = ObjectMap(s, t)

    def maps(d: List[String]) = mapsto(d)
    def mapsto(d: String*): ObjectData = mapsto(d.toList)
    def mapsto(d: List[String]): ObjectData = ObjectData(s, d)
  }
  implicit def arrowAsMorphismSource(s: Arrow) = new MorphismSource(s)
  class MorphismSource(s: Arrow) {
    def maps(t: Arrow) = mapsto(t)
    def mapsto(t: Arrow) = new MorphismMap(s, t)
    def maps(d: Map[String, String]) = mapsto(d)
    def mapsto(d: (String, String)*): MorphismData = mapsto(d.toMap)
    def mapsto(d: Map[String, String]): MorphismData = MorphismData(s, d)
  }

  trait Set
  object Set extends Set

  def functor(source: Ontology, target: Ontology, onObjects: String => String, onMorphisms: Arrow => Path): Translation = ???

  def dataset(source: Ontology, onObjects: String => List[String], onMorphisms: Arrow => (String => String)): Dataset = ???
}

