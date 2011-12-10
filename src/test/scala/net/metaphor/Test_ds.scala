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

  /* 
   * GRAPHS!
   */
  
  val Grph = category(free
      having Objects ("a vertex","an edge")
      having Morphisms(
          "an edge"---"has as source"-->"a vertex"
          "an edge"---"has as target"-->"a vertex"
      )
  )
 
  val termGraph = dataset (Grph==>Set) (
      onObjects
      	"an edge" mapsto rowset ("+1")
      	"a vertex" mapsto rowset ("N")
  )(
      onMorphisms
      	"an edge"---"has as source"-->"a vertex" mapsto table (
      			"+1" -> "N"
    		  )
      	"an edge"---"has as target"-->"a vertex" mapsto table (
      			"+1" -> "N"
    		  )
  )
  
  val termBigraph = dataset (Grph==>Set) (
      onObjects
      	"an edge" mapsto rowset ("input","output")
      	"a vertex" mapsto rowset ("species","transition")
  )(
      onMorphisms
      	"an edge"---"has as source"-->"a vertex" mapsto table (
      			"input" -> "species"
      			"output"-> "transition"
    		  )
      	"an edge"---"has as target"-->"a vertex" mapsto table (
      			"input" -> "transition"
      			"output"-> "species"
    		  )
  )
  
  val initgraph = dataset (Grph==>Set) (
      onObjects
      	"an edge" mapsto rowset ()
      	"a vertex" mapsto rowset ()
  )(
      onMorphisms
      	"an edge"---"has as source"-->"a vertex" mapsto (
    		  )
      	"an edge"---"has as target"-->"a vertex" mapsto (
    		  )
  )
  
  val DavidsFunkyGraph = dataset (Grph==>Set) (
      "an edge" mapsto rowset ("f","g","h","i","j")
      "a vertex" mapsto rowset ("A","B","C","D")
  )(
      "an edge"---"has as source"-->"a vertex" mapsto table (
    	"f" -> "A"
    	"g" -> "A"
    	"h" -> "B"
    	"i" -> "A"
    	"j" -> "C"
    	"k" -> "C"
    		  )
      "an edge"---"has as target"-->"a vertex" mapsto table (
    	"f" -> "B"
    	"g" -> "B"
    	"h" -> "C"
    	"i" -> "C"
    	"j" -> "C"
    	"k" -> "C"
    		  )
  )
   /* 
   * Ordinals
   * TO DO: Some kind of schematic for Ord[n].
   */
  val Ord[0] = category(free
      having Objects ("V0")
      having Morphisms ()
  )
  
  val Ord[1] = category(free
      having Objects ("V0","V1")
      having Morphisms ("V0"---"E01"-->"V1")
  )
  
  val Ord[2] = category(free
      having Objects ("V0","V1","V2")
      having Morphisms (
    		  "V0"---"E01"-->"V1"
    		  "V1"---"E12"-->"V2"
      )
  )

  val Ord[3] = category(free
      having Objects ("V0","V1","V2","V3")
      having Morphisms (
    		  "V0"---"E01"-->"V1"
    		  "V1"---"E12"-->"V2"
    		  "V2"---"E23"-->"V3"
      )
  )
  
  val termCat = Ord[0]
  
  val Compose = functor (Ord[1]==>Ord[2])(
      onObjects
      	"V0" mapsto "V0",
      	"V1" mapsto "V2"
      	)(
      onMorphisms(
      	"V0"---"E01"-->"V1" mapsto "V0"---"E01"-->"V1"---"E12"-->"V2"
      )
  )
      	   
      
  
  val PickOutSource = functor (Ord[1] ==> Grph)(
   	onObjects
   		"V0" mapsto "an edge"
   		"V1" mapsto "a vertex"
   )(
    onMorphisms
    	"V0"---"E01"-->"V1" mapsto "an edge"---"has as source"-->"a vertex"
  )
  
  val PickOutTarget = functor (Ord[1] ==> Grph)(
   	onObjects
   		"V0" mapsto "an edge"
   		"V1" mapsto "a vertex"
   )(
    onMorphisms
    	"V0"---"E01"-->"V1" mapsto "an edge"---"has as target"-->"a vertex"
  )
  
  val DiscreteDynamicalSystem = category(free
      having Objects ("an element")
      having Morphisms ("an element"---"has as successor"-->"an element")
  )
  
  val ReverseGraph = functor (Grph ==> Grph)(
	onObjects
		"an edge" mapsto "an edge"
		"a vertex" mapsto "a vertex"
  )(
    onMorphisms
    	"an edge"---"has as source"-->"a vertex" mapsto "an edge"---"has as target"-->"a vertex"
    	"an edge"---"has as target"-->"a vertex" mapsto "an edge"---"has as source"-->"a vertex"
  )
  
  /* 
   * I needed an understanding of paths in GraphToDDS1. 
   * Specifically, I want to specify that a morphism in Grph maps to an identity morphism in DiscreteDynamicalSystem
   * I used the following syntax: path0(X)   to mean: the identity morphism on object X
   */
  
  val GraphToDDS1 = functor (Grph ==> DiscreteDynamicalSystem)(
	onObjects
		"an edge" mapsto "an element"
		"a vertex" mapsto "an element"
  )(
    onMorphisms
    	"an edge"---"has as source"-->"a vertex" mapsto (path0 ("an element")),
    	"an edge"---"has as target"-->"a vertex" mapsto "an element"---"has as successor"-->"an element"
  )
  
  val GraphToDDS2 = functor (Grph ==> DiscreteDynamicalSystem)(
	onObjects
		"an edge" mapsto "an element"
		"a vertex" mapsto "a vertex"
  )(
    onMorphisms
    	"an edge"---"has as source"-->"a vertex" mapsto "an edge"---"has as target"-->"a vertex"
    	"an edge"---"has as target"-->"a vertex" mapsto (path0 ("an element"))
  )
  
  val IntsMod2Group = category(
      having Objects ("an element")
      having Morphisms ("an element"---"is married to"-->"an element")
      having Relations (
          ("an element"---"is married to"-->"an element"---"is married to"-->"an element") 
          equivalently 
          (path0 ("an element"))
      )
  )
  
  val IntsMod2Groupoid = category(
      having Objects ("0","1")
      having Morphisms (
          "0"---"next"-->"1"
          "1"---"next"-->"0")
      having Relations (
          ("0"---"next"-->"1"---"next"-->"0") 
          equivalently as ("0","0")
          (path0 ("0"))
          
          ("1"---"next"-->"0"---"next"-->"1") 
          equivalently as ("1","1")
          (path0 ("1"))
      )
  )
  
  val Section = category(
      having Objects ("a pointed set","an element")
      having Morphisms (
          "an element"---"is in"-->"a pointed set"
          "a pointed set"---"has as chosen"-->"an element")
      having Relations (
          ("a pointed set"---"has as chosen"-->"an element"---"has"-->"a pointed set") 
          equivalently as ("a pointed set","a pointed set")
          (path0 ("a pointed set"))
      )
  ))   
  
      
  val d = ???
  val e = ??? 
  
  "pushforward" should "work" in {
    F._*(d) should equal (e)
  }
  
  "shriek" should "work" in {
    1+2 should equal (4)
  }
  		
  
}
