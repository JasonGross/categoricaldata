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
  
  
//  /* 
//   * GRAPHS!
//   */
//	// NOTE, need commas between lines
//  val Grph = model(
//      "an edge"---"has as source"-->"a vertex",
//      "an edge"---"has as target"-->"a vertex"
//  )
// 
//  val termGraph = dataset (Grph==>Set) (
//      "an edge" mapsto ("+1"),
//      "a vertex" mapsto ("N")
//  )(
//      "an edge"---"has as source"-->"a vertex" mapsto (
//    	"+1" -> "N"
//    		  ),
//      "an edge"---"has as target"-->"a vertex" mapsto (
//    	"+1" -> "N"
//    		  )
//  )
//  
//  val termBigraph = dataset (Grph==>Set) (
//      "an edge" mapsto ("input","output"),
//      "a vertex" mapsto ("species","transition")
//  )(
//      "an edge"---"has as source"-->"a vertex" mapsto (
//    	"input" -> "species",
//    	"output"-> "transition"
//    		  ),
//      "an edge"---"has as target"-->"a vertex" mapsto (
//    	"input" -> "transition",
//    	"output"-> "species"
//    		  )
//  )
//  
//  val initgraph = dataset (Grph==>Set) (
//      "an edge" mapsto (),
//      "a vertex" mapsto ()
//  )(
//      "an edge"---"has as source"-->"a vertex" mapsto (
//    		  ),
//      "an edge"---"has as target"-->"a vertex" mapsto (
//    		  )
//  )
//  
//  val DavidsFunkyGraph = dataset (Grph==>Set) (
//      "an edge" mapsto ("f","g","h","i","j"),
//      "a vertex" mapsto ("A","B","C","D")
//  )(
//      "an edge"---"has as source"-->"a vertex" mapsto (
//    	"f" -> "A",
//    	"g" -> "A",
//    	"h" -> "B",
//    	"i" -> "A",
//    	"j" -> "C",
//    	"k" -> "C"
//    		  ),
//      "an edge"---"has as target"-->"a vertex" mapsto (
//    	"f" -> "B",
//    	"g" -> "B",
//    	"h" -> "C",
//    	"i" -> "C",
//    	"j" -> "C",
//    	"k" -> "C"
//    		  )
//  )
//    
//  val D = ???
//  val F = ???
//  val d = ???
//  val e = ??? 
//  
//  "pushforward" should "work" in {
////    F._*(d) should equal (e)
//  }
//  
//  "shriek" should "work" in {
//    1+2 should equal (4)
//  }
  		
  
}
