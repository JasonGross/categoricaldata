package net.metaphor.dsl
import net.metaphor.api.Ontology
import net.metaphor.api.Translation
import net.metaphor.api.Dataset

object Sentences2 {
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

  def model(contents: Arrow*): Ontology = ???

  object Boxes {
    def apply(boxes: String*) = BoxesDescription(boxes.toList)
    def apply(boxMap: (String, String)*) = BoxesMapDescription(boxMap.toMap)
    def apply(boxData: (String, List[String])*) = BoxesDataDescription(boxData.toMap)
  }
  object Arrows {
    def apply(arrows: Arrow*) = ArrowsDescription(arrows.toList)
    def apply(arrowMap: (Arrow, Path)*) = ArrowsMapDescription(arrowMap.toMap)
    def apply(arrowData: (Arrow, Map[String, String])*) = ArrowsDataDescription(arrowData.toMap)
  }
  
  case class BoxesDescription(boxes: List[String])

  case class ArrowsDescription(arrows: List[Arrow])

  val Category = CategoryDescription(BoxesDescription(Nil), ArrowsDescription(Nil))
  
  case class CategoryDescription(val boxes: BoxesDescription, val arrows: ArrowsDescription) {
    def having(boxes: BoxesDescription): CategoryDescription = copy(boxes = boxes)
    def having(arrows: ArrowsDescription): CategoryDescription = copy(arrows = arrows)
  }

  
  case class BoxesMapDescription(boxPairs: Map[String, String])
  case class ArrowsMapDescription(arrowsToPaths: Map[Arrow, Path])
  
  val Functor = FunctorDescription(BoxesMapDescription(Map()), ArrowsMapDescription(Map()))
  
  case class FunctorDescription(val boxMap: BoxesMapDescription, val arrowMap: ArrowsMapDescription) {
    def on(boxMap: BoxesMapDescription): FunctorDescription = copy(boxMap = boxMap)
    def on(arrowMap: ArrowsMapDescription): FunctorDescription = copy(arrowMap = arrowMap)
  }

  val Dataset = DatasetDescription(BoxesDataDescription(Map()), ArrowsDataDescription(Map()))
  
  case class DatasetDescription(val boxData: BoxesDataDescription, val arrowData: ArrowsDataDescription) {
    def on(boxData: BoxesDataDescription): DatasetDescription = copy(boxData = boxData)
    def on(arrowData: ArrowsDataDescription): DatasetDescription = copy(arrowData = arrowData)    
  }

  case class BoxesDataDescription(boxPairs: Map[String, List[String]])
  case class ArrowsDataDescription(arrowsToPaths: Map[Arrow, Map[String, String]])

}

