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

  val termBigraph = dataset (source = Grph,
    onObjects = Map (
    	"an edge" -> List ("input", "output"),
    	"a vertex" -> List ("species", "transition")),
    onMorphisms = Map (
    	("an edge" --- "has as source" --> "a vertex") -> Map (
    		"input" -> "species",
    		"output" -> "transition"),
    	("an edge" --- "has as target" --> "a vertex") -> Map (
    		"input" -> "transition",
    		"output" -> "species"))
   )  
   
  val initGraph = dataset (source = Grph,
    onObjects = Map (
        "an edge" -> List(),
        "a vertex" -> List()),
    onMorphisms = Map(
    	("an edge" --- "has as source" --> "a vertex") -> Map(),
    	("an edge" --- "has as target" --> "a vertex") -> Map())
  )

  val DavidsFunkyGraph = dataset(source = Grph,
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
   
  /* 
   * Ordinals
   * TO DO: Some kind of schematic for Ord[n].
   */
  val Ord0 = category(
	objects =List ("V0"),
    arrows = List ()
  )
  
  val Ord1 = category(
    objects = List ("V0","V1"),
    arrows =List ("V0"---"E01"-->"V1")
  )
  
  val Ord2 = category(
    objects = List ("V0","V1","V2"),
    arrows = List (
    		"V0"---"E01"-->"V1",
    		"V1"---"E12"-->"V2")
  )

  val Ord3 = category(
    objects = List ("V0","V1","V2","V3"),
    arrows = List (
    		  "V0"---"E01"-->"V1",
    		  "V1"---"E12"-->"V2",
    		  "V2"---"E23"-->"V3")
      
  )

  def Ord(n: Int) = category(
    objects = for (i <- 0 to n) yield "V" + i,
    arrows = for (i <- 0 to n - 1) yield {
      ("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString)
    })
 
  
  val Domain = functor (
    source = Ord0,
    target = Ord1,
	onObjects = Map ("V0" -> "V0"),
	onMorphisms = Map ()
  )
 
  val Codomain = functor (
    source = Ord0,
    target = Ord1,
	onObjects = Map ("V0" -> "V1"),
	onMorphisms = Map ()
  )
	  
  
      
  val Compose = functor (
  	source = Ord1,
    target = Ord2,
	onObjects = Map (
	    "V0" -> "V0",
	    "V1" -> "V2"),
	onMorphisms = Map (
	    ("V0"---"E01"-->"V1") -> ("V0"---"E01"-->"V1"---"E12"-->"V2"))
  )
      	   
  val terminalCat = Ord0

  "pushforward" should "work" in {
    //    F._*(d) should equal (e)
  }

  "shriek" should "work" in {
    1 + 2 should equal(4)
  }

}
