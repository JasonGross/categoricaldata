//package net.metaphor
//
//import org.scalatest.FlatSpec
//import org.scalatest.matchers.ShouldMatchers
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
//
//import scala.math._
//
///*
// * Currently no expectation that this code compiles.
// */
//
//@RunWith(classOf[JUnitRunner])
//class Test extends FlatSpec with ShouldMatchers {
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

