package net.metaphor.dsl
import net.metaphor.api.Ontology
import net.metaphor.api.Translation

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
	
	def model(contents: Arrow*): Ontology = ???
	
	case class ObjectMap(source: String, target: String)
	
	implicit def stringAsObjectMapSource(s: String) = new ObjectMapSource(s)
	class ObjectMapSource(s: String) {
	  def maps = new PartialObjectMap
	  
	  class PartialObjectMap {
	    def to(t: String) = ObjectMap(s, t)
	  }
	}
	
	case class MorphismMap(source: Arrow, target: Path)
	implicit def arrowAsMorphismMapSource(s: Arrow) = new MorphismMapSource(s)
	
	class MorphismMapSource(s: Arrow) {
	  def maps = new PartialMorphismMap
	  class PartialMorphismMap {
	    def to(t: Arrow) = new MorphismMap(s, t)
	  }
	}
	
	implicit def ontologyAsFunctorSource(o: Ontology) = new FunctorSource(o)
	class FunctorSource(s: Ontology) {
	  def ==>(t: Ontology) = (s, t)
	}
	
	def functor(sourceAndTarget: (Ontology, Ontology))(onObjects: ObjectMap*)(onMorphisms: MorphismMap*): Translation = ???
}

