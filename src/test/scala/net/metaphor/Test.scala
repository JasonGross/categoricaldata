package net.metaphor

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

/*
 * Currently no expectation that this code compiles.
 */

@RunWith(classOf[JUnitRunner])
class Test extends FlatSpec with ShouldMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._

  /* 
   * GRAPHS!
   */
  // NOTE, need commas between lines
  
 
  val Grph = category (
    objects = List("an edge", "a vertex"),
    arrows = List(
      "an edge" --- "has as source" --> "a vertex",
      "an edge" --- "has as target" --> "a vertex")
  )

  
        
  val termGraph = dataset (source = Grph,
    onObjects = Map(
        "an edge" -> List("+1"),
        "a vertex" -> List("N")),
    onMorphisms = Map(
        ("an edge" --- "has as source" --> "a vertex") -> Map ("+1" -> "N"),
        ("an edge" --- "has as target" --> "a vertex") -> Map ("+1" -> "N"))
  )

  val termBigraph = dataset(source = Grph,
    onObjects = Map(
    	"an edge" -> List ("input", "output"),
    	"a vertex" -> List ("species", "transition")),
    onMorphisms = Map(
    	("an edge" --- "has as source" --> "a vertex") -> Map (
    		"input" -> "species",
    		"output" -> "transition"),
    	("an edge" --- "has as target" --> "a vertex") -> Map (
    		"input" -> "transition",
    		"output" -> "species"))
   )

  val initgraph = dataset (source = Grph
    onObjects = Map (
        "an edge" -> Nil,
        "a vertex" -> Nil),
    onMorphisms = Map(
    	("an edge" --- "has as source" --> "a vertex") -> Map(),
    	("an edge" --- "has as target" --> "a vertex") -> Map())
  )

  val DavidsFunkyGraph = dataset(source = Grph
    onObjects = Map (
        "an edge" -> List ("f", "g", "h", "i", "j"),
        "a vertex" -> List ("A", "B", "C", "D")),
    onMorphisms = Map ( 
    	("an edge" --- "has as source" --> "a vertex") -> Map (
    			"f" -> "A",
    			"g" -> "A",
    			"h" -> "B",
    			"i" -> "A",
    			"j" -> "C",
    			"k" -> "C"),
        ("an edge" --- "has as target" --> "a vertex") -> Map (
        		"f" -> "B",
        		"g" -> "B",
        		"h" -> "C",
        		"i" -> "C",
        		"j" -> "C",
        		"k" -> "C"))
   )

  "pushforward" should "work" in {
    //    F._*(d) should equal (e)
  }

  "shriek" should "work" in {
    1 + 2 should equal(4)
  }

}
