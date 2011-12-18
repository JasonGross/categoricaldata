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
  
 
  val Grph = ontology (
    objects = List("an edge", "a vertex"),
    arrows = List(
      "an edge" --- "has as source" --> "a vertex",
      "an edge" --- "has as target" --> "a vertex")
  )

  
        
  val terminalGraph = dataset (source = Grph,
    onObjects = Map(
        "an edge" -> List("+1"),
        "a vertex" -> List("N")),
    onMorphisms = Map(
        ("an edge" --- "has as source" --> "a vertex") -> Map ("+1" -> "N"),
        ("an edge" --- "has as target" --> "a vertex") -> Map ("+1" -> "N"))
  )

  val terminalBigraph = dataset (source = Grph,
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
   
  val initialGraph = dataset (source = Grph,
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
  val Ord0 = ontology(
	objects =List ("V0"),
    arrows = List ()
  )
  
  val Ord1 = ontology(
    objects = List ("V0","V1"),
    arrows =List ("V0"---"E01"-->"V1")
  )
  
  val Ord2 = ontology(
    objects = List ("V0","V1","V2"),
    arrows = List (
    		"V0"---"E01"-->"V1",
    		"V1"---"E12"-->"V2")
  )

  val Ord3 = ontology(
    objects = List ("V0","V1","V2","V3"),
    arrows = List (
    		  "V0"---"E01"-->"V1",
    		  "V1"---"E12"-->"V2",
    		  "V2"---"E23"-->"V3")
      
  )

  def Ord(n: Int) = ontology(
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
	onMorphisms = Map ())
  
  
  // these need to be "def" not "val", in order to take parameters
  def Skip (n : Int, k : Int) = functor (
      source = Ord(n),
      target = Ord(n+1),
      onObjects = Map ( 
          for (i <- 0 to k-1) yield "V" + i.toString + "->" + "V" + i.toString,
          for (i <- k to n) yield "V" + i.toString + "->" + "V" + (i + 1).toString)
      onMorphisms = Map (
          for (i <- 0 to k-1) yield {
          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
          	->
          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))},
          (("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString))
          ->
          (("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString) --- ("E" + (k + 1).toString + (k + 2).toString) --> ("V" + (k + 2).toString)),
          for (i <- k+1 to n-1) yield {
            (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
          	->
          	(("V" + (i+1).toString) --- ("E" + (i + 1).toString + (i + 2).toString) --> ("V" + (i + 2).toString))}))

  
  def Coface(n: Int, k: Int) = Skip (n,k)

  def Duplicate(n : Int, k : Int) = functor (
      source = Ord(n),
      target = Ord(n-1),
      onObjects = Map ( 
          for (i <- 0 to k) yield "V" + i.toString + "->" + "V" + i.toString,
          for (i <- k+1 to n) yield "V" + i.toString + "->" + "V" + (i - 1).toString)
      onMorphisms = Map (
          for (i <- 0 to k-1) yield {
          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
          	->
          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))},
          (("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString))
          ->
          ("V" + k.toString),
          for (i <- k+1 to n-1) yield {
            (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
          	->
          	(("V" + (i - 1).toString) --- ("E" + (i - 1).toString + i.toString) --> ("V" + i.toString))}))
  
  def Codegeneracy(n : Int, k : Int) = Duplicate (n,k)
  
 
  val Compose = functor (
  	source = Ord1,
    target = Ord2,
	onObjects = Map (
	    "V0" -> "V0",
	    "V1" -> "V2"),
	onMorphisms = Map (
	    ("V0"---"E01"-->"V1") -> ("V0"---"E01"-->"V1"---"E12"-->"V2"))
  )
      	   
  val termCat = Ord0
  
  def terminalFunctor(c : Ontology) = functor (
      source = c,
      target = terminalCat,
      onObjects = Map( for (b <- c.boxes) yield (b.name -> "V0")),
      onMorphisms = Map (for (b <- c.arrows) yield (b.source.name --- b.name --> b.target.name))))
      	
    
  
  
      
  
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
  
 
  val DiscreteDynamicalSystem = ontology(
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
  
   val IntsMod2Group = ontology(
      objects = List ("an element"),
      arrows = List ("an element"---"is married to"-->"an element"),
      relations =List (
          ("an element"---"is married to"-->"an element"---"is married to"-->"an element") 
          === 
          ("an element")))

  
  val IntsMod2Groupoid = ontology(
      objects = List ("0","1"),
      arrows = List (
          "0"---"next"-->"1",
          "1"---"next"-->"0"),
      relations (
          (("0"---"next"-->"1"---"next"-->"0") 
          === 
          ("0")),
          (("1"---"next"-->"0"---"next"-->"1") 
          ===
          ("1"))))
          
  val PointedSets = ontology(
      objects = List ("a pointed set", "an element"),
      arrows = List (
          "an element"---"is in"-->"a pointed set",
          "a pointed set"---"has as chosen"-->"an element"),
      relations = List (
          ("a pointed set"---"has as chosen"-->"an element"---"is in"-->"a pointed set") 
          === 
          ("a pointed set")))
  


      
  val DavidsFunkyFunction = dataset(source = Ord(1),
      onObjects = Map (
         "V0" -> List ("David","Alex Crujeiras","Scott","UC Berkeley", "MIT"),
      	 "V1" -> List ("1978","Scott's birthyear", "1868","1861")),
      onMorphisms (
          "V0"---"E01"-->"V1" -> Map (
              "David" -> "1978",
              "Alex Crujeiras" -> "1978",
              "Scott" -> "Scott's birthyear",
              "UC Berkeley" -> "1868",
              "MIT" -> "1861")))
              
  val DavidsFunkySet1 = dataset(source = Ord(0),
      onObjects = Map (
          "V0" -> List ("David","Scott","UC Berkeley", "MIT")),
      onMorphisms())
  
  val DavidsFunkySet2 = dataset(source = Ord(0),
      onObjects = Map (
          "V0" -> List ("1978","Scott's birthyear", "1868","1861")),
      onMorphisms())
  
  val Drawers = dataset(Ord(1),
      onObjects = Map (
         "V0" -> List ("Item 1","Item 2","Item 3", "Item 4"),
      	 "V1" -> List ("Top Drawer","Bottom Drawer")),
      onMorphisms = Map (
          "V0"---"E01"-->"V1" -> Map (
              "Item 1" -> "Top Drawer",
              "Item 2" -> "Bottom Drawer",
              "Item 3" -> "Top Drawer",
              "Item 4" -> "Top Drawer")))
   
  
  
      
  "pullback" should "work" in {
    Domain.^*(DavidsFunkyFunction) should equal (DavidsFunkySet1)
  }
  
   "pullback" should "work" in {
    Codomain.^*(DavidsFunkyFunction) should equal (DavidsFunkySet2)
  }
      
   "pushforward" should "work" in {
    ConstantOrd[1]._*(DavidsFunkyFunction) should equal (DavidsFunkySet1)
  }
  
   "shriek" should "work" in {
    ConstantOrd[1]._!(DavidsFunkyFunction) should equal (DavidsFunkySet2)
  }
  		
  


