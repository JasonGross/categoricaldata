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
  
  val Grph = model(free
      having Objects ("a vertex","an edge")
      having Morphisms(
          "an edge"---"has as source"-->"a vertex",
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
  
  val Domain = functor (Ord[0]==>Ord[1])(
		  onObjects "V0" mapsto "V0"
		  onMorphisms empty
      )
 
  val Codomain = functor (Ord[0]==>Ord[1])(
		  onObjects "V0" mapsto "V1"
	  )
	  
  
      
  val Compose = functor (Ord[1]==>Ord[2])(
      onObjects
      	"V0" mapsto "V0",
      	"V1" mapsto "V2"
      	)(
      onMorphisms(
      	"V0"---"E01"-->"V1" mapsto "V0"---"E01"-->"V1"---"E12"-->"V2"
      )
  )
      	   
  val terminalCat = Ord[0]
  
  /* For any category X, there is a unique functor X==>terminalCat. 
   * I used the term "obvious" here. Scott can deal with this issue in any way he sees fit.  
   */
  
  val constantX=functor (X==>terminalCat)(
      obvious)
      
  
  val SourceFunction = functor (Ord[1] ==> Grph)(
   	onObjects
   		"V0" mapsto "an edge"
   		"V1" mapsto "a vertex"
   )(
    onMorphisms
    	"V0"---"E01"-->"V1" mapsto "an edge"---"has as source"-->"a vertex"
  )
  
  val TargetFunction = functor (Ord[1] ==> Grph)(
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
  
  val PointedSets = category(
      having Objects ("a pointed set","an element")
      having Morphisms (
          "an element"---"is in"-->"a pointed set"
          "a pointed set"---"has as chosen"-->"an element")
      having Relations (
          ("a pointed set"---"has as chosen"-->"an element"---"is in"-->"a pointed set") 
          equivalently as ("a pointed set","a pointed set")
          (path0 ("a pointed set"))
      )
  )   
  
      
  val DavidsFunkyFunction = dataset(Ord[1]==>Set) (
      onObjects (
         "V0" mapsto rowset ("David","Alex Crujeiras","Scott","UC Berkeley", "MIT")
      	 "V1" mapsto rowset ("1978","Scott's birthyear", "1868","1861") 
      	  
      onMorphisms (
          "V0"---"E01"-->"V1" mapsto table (
              "David" -> "1978",
              "Alex Crujeiras" -> "1978"
              "Scott" -> "Scott's birthyear",
              "UC Berkeley" -> "1868",
              "MIT" -> "1861"
              )
  )
  
  val DavidsFunkySet1 = dataset(Ord[0]==>Set)(
      onObjects (
          "V0" mapsto rowset ("David","Scott","UC Berkeley", "MIT")
      onMorphisms()
      )
  
  val DavidsFunkySet2 = dataset(Ord[0]==>Set)(
      onObjects (
          "V0" mapsto rowset ("1978","Scott's birthyear", "1868","1861")
      onMorphisms()
      )
  
  val Drawers = dataset(Ord[1]==>Set)(
      onObjects (
         "V0" mapsto rowset ("Item 1","Item 2","Item 3", "Item 4")
      	 "V1" mapsto rowset ("Top Drawer","Bottom Drawer") 
      	  
      onMorphisms (
          "V0"---"E01"-->"V1" mapsto table (
              "Item 1" -> "Top Drawer",
              "Item 2" -> "Bottom Drawer",
              "Item 3" -> "Top Drawer",
              "Item 4" -> "Top Drawer",
              )
  )
  
  /* Feel free to delete for now. But at some point, it would be cool to somehow encode topological categories.*/
  
  val FundGrpdS1 = topologicalCategory(
	having Objects (for theta in [0,1) "clockhand"<theta> )
	having Morphisms (
	    for t in realNumbers
	    for theta in [0,1)
         	"clockhand"<theta> ---"duration"<t>-->"clockhand"(<theta>+<t>) )
    having Relations (
	    for theta in [0,1)
        for t1 in realNumbers
        for t2 in realNumbers
         		("clockhand"<theta> ---"duration"<t1>-->"clockhand"(<theta>+<t1>)"---"duration"<t2>-->"clockhand"(<theta>+<t1>+<t2>)) 
	    		equivalently as ("clockhand"<theta>,"clockhand"(<theta>+<t1>+<t2>))
	    		("clockhand"<theta> ---"duration"(<t1>+<t2>)-->"clockhand"(<theta>+<t1>+<t2>)") 
	    			
  )    
      
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
  		
  

