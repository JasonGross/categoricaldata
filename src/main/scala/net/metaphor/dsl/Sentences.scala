package net.metaphor.dsl
import net.metaphor.api.Ontology
import net.metaphor.api.Translation
import net.metaphor.api.Dataset

object Sentences {
  implicit def stringAsArrowSource(s: String) = new ArrowSource(s)
  implicit def arrowAsPath(a: Arrow) = new Path(List(a))
  
  case class ArrowSource(s: String) {
    def ---(p: String) = new PartialArrow(p)
    class PartialArrow(p: String) {
      def -->(o: String) = new Arrow(s, p, o)
    }
  }

  case class Arrow(s: String, p: String, o: String) {
    println(s + " " + p + " " + o)
    def ---(p: String) = new IncompletePath(new Path(List(this)), p)
  }

  case class IncompletePath(path: Path, p: String) {
    def --->(o: String) = new Path(path.arrows ::: List(new Arrow(path.arrows.last.o, p, o)))
  }
  
  case class Path(arrows: List[Arrow]) {
    def ---(p: String) = new IncompletePath(this, p)
  }
  
  def model(objects: List[String], arrows: List[Arrow]): Ontology = ???

  def functor(source: Ontology, target: Ontology, onObjects: String => String, onMorphisms: Arrow => Path): Translation = ???

  def dataset(source: Ontology, onObjects: String => List[String], onMorphisms: Arrow => (String => String)): Dataset = ???
}

