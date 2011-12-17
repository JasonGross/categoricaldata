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
    objects = for (i <- 0 to n) yield "V" + i.toString, //David added the ".toString" here. Correct?
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
  
  
  val Skip(n : Int, k : Int) = functor (
      source = Ord(n),
      target = Ord(n+1),
      onObjects = Map ( 
          for (i <- 0 to k-1) yield "V" + i.toString + "->" + "V" + i.toString,
          for (i <- k to n) yield "V" + i.toString + "->" + "V" + (i + 1).toString)
      onMorphisms = Map (
          for (i <- 0 to k-1) yield (
          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
          	->
          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))),
          (("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString))
          ->
          (("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString) --- ("E" + (k + 1).toString + (k + 2).toString) --> ("V" + (k + 2).toString)),
          for (i <- k+1 to n-1) yield (
            (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
          	->
          	(("V" + (i+1).toString) --- ("E" + (i + 1).toString + (i + 2).toString) --> ("V" + (i + 2).toString))))
  )
  
  val Coface(n: Int, k: Int) = Skip (n,k)

  val Duplicate(n : Int, k : Int) = functor (
      source = Ord(n),
      target = Ord(n-1),
      onObjects = Map ( 
          for (i <- 0 to k) yield "V" + i.toString + "->" + "V" + i.toString,
          for (i <- k+1 to n) yield "V" + i.toString + "->" + "V" + (i - 1).toString)
      onMorphisms = Map (
          for (i <- 0 to k-1) yield (
          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
          	->
          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))),
          (("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString))
          ->
          ("V" + k.toString),
          for (i <- k+1 to n-1) yield (
            (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
          	->
          	(("V" + (i - 1).toString) --- ("E" + (i - 1).toString + i.toString) --> ("V" + i.toString))))
  )
  
  val Codegeneracy(n : Int, k : Int) = Duplicate (n,k)
  
 
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
  
   /* For any category X, there is a unique functor X==>terminalCat. 
   * I used the term "obvious" here. Scott can deal with this issue in any way he sees fit.  
   */
  
  
      
  
  val SourceFunction = functor (
    source = Ord1,
    target = Grph,
   	onObjects = Map (
   		"V0" -> "an edge",
   		"V1" -> "a vertex"),
    onMorphisms = Map (
    	("V0"---"E01"-->"V1") -> ("an edge"---"has as source"-->"a vertex"))
  )
  
  val TargetFunction = functor (
   	source = Ord1,
   	target = Grph,
   	onObjects = Map (
   		"V0" -> "an edge",
   		"V1" -> "a vertex"),
   	onMorphisms = Map (
    	("V0"---"E01"-->"V1") -> ("an edge"---"has as target"-->"a vertex"))
  )
  
 
  val DiscreteDynamicalSystem = category(
    objects = List ("an element"),
    arrows = List ("an element"---"has as successor"-->"an element")
  )
  
  val ReverseGraph = functor (
    source = Grph,
    target = Grph,
	onObjects = Map (
		"an edge" -> "an edge",
		"a vertex" -> "a vertex"),
    onMorphisms = Map (
    	("an edge"---"has as source"-->"a vertex") -> ("an edge"---"has as target"-->"a vertex"),
    	("an edge"---"has as target"-->"a vertex") -> ("an edge"---"has as source"-->"a vertex"))
  )
  
  /* 
   * I needed an understanding of paths in GraphToDDS1. 
   * Specifically, I want to specify that a morphism in Grph maps to an identity morphism in DiscreteDynamicalSystem
   * I used the following syntax: path0(X)   to mean: the identity morphism on object X
   */
  
  val GraphToDDS1 = functor (
    source = Grph,
    target = DiscreteDynamicalSystem,
	onObjects = Map (
		"an edge" -> "an element",
		"a vertex" -> "an element"),
    onMorphisms = Map (
    	("an edge"---"has as source"-->"a vertex") -> ("an element"),
    	("an edge"---"has as target"-->"a vertex") -> ("an element"---"has as successor"-->"an element"))
  )
  
  val GraphToDDS2 = functor (
    source = Grph, 
    target = DiscreteDynamicalSystem,
	onObjects = Map (
		"an edge" -> "an element",
		"a vertex" -> "a vertex"),
    onMorphisms = Map (
    	("an edge"---"has as source"-->"a vertex") -> ("an edge"---"has as target"-->"a vertex"),
    	("an edge"---"has as target"-->"a vertex") -> (identity("an element")))
  )
  
  "pushforward" should "work" in {
    //    F._*(d) should equal (e)
  }

  "shriek" should "work" in {
    1 + 2 should equal(4)
  }

}
