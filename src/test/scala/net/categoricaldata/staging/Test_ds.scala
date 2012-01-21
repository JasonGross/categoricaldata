package net.categoricaldata

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

/*
 * EVERYTHING MUST COMPILE
 */

//@RunWith(classOf[JUnitRunner])
//class Test extends FlatSpec with ShouldMatchers {
//
//   "RightPush of Pullback" should "be isomorphic to original graph for BipartiteGraphToGraph" in {
//     val X=GraphDataset120114
//     val F=Examples.BipartiteGraphToGraph
//     val PB=Examples.BipartiteGraphToGraph.^*(X)
//     val Final=Examples.BipartiteGraphToGraph.__*(PB)
//     println("Original Graph: "+X)
//     println("Right pushforward of pullback of original graph: "+ Final)
//   }  
//
//     val GraphDataset120114 = Dataset(source = Examples.Graph,
//    onObjects = Map(
//      "an edge" -> List("f", "g", "h", "i", "j"),
//      "a vertex" -> List("A", "B", "C", "D")),
//    onMorphisms = Map(
//      ("an edge" --- "has as source" --> "a vertex") -> Map(
//        "f" -> "B",
//        "g" -> "B",
//        "h" -> "C",
//        "i" -> "C",
//        "j" -> "C"),
//      ("an edge" --- "has as target" --> "a vertex") -> Map(
//        "f" -> "A",
//        "g" -> "A",
//        "h" -> "B",
//        "i" -> "A",
//        "j" -> "C")))
//  
  //  /* 
//   * Topological categories
//   */
//  
// val FundGrpdS1 = topologicalCategory(
//	having Objects (for theta in [0,1) "clockhand"<theta> )
//	having Morphisms (
//	    for t in realNumbers
//	    for theta in [0,1)
//         	"clockhand"<theta> ---"duration"<t>-->"clockhand"(<theta>+<t>) )
//    having Relations (
//	    for theta in [0,1)
//        for t1 in realNumbers
//        for t2 in realNumbers
//         		("clockhand"<theta> ---"duration"<t1>-->"clockhand"(<theta>+<t1>)"---"duration"<t2>-->"clockhand"(<theta>+<t1>+<t2>)) 
//	    		equivalently as ("clockhand"<theta>,"clockhand"(<theta>+<t1>+<t2>))
//	    		("clockhand"<theta> ---"duration"(<t1>+<t2>)-->"clockhand"(<theta>+<t1>+<t2>)") 
//	    			
//  )    

