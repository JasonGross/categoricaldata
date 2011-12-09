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

  val C = ???
  val D = ???
  val F = ???
  val d = ???
  val e = ??? 
  
  "pushforward" should "work" in {
    F._*(d) should equal (e)
  }
  
  "shriek" should "work" in {
    1+2 should equal (4)
  }
  		
  
}
