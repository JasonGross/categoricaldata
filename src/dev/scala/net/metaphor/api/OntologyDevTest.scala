package net.metaphor.api

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.metaphor.examples.Examples
import net.metaphor.CustomMatchers
import net.tqft.toolkit.arithmetic.Factorial

@RunWith(classOf[JUnitRunner])
class OntologyDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._
  
  "FiniteCyclicMonoid.morphismsOfLength" should "have the right sizes" in { 
	  val C = Examples.FiniteCyclicMonoid(5, 3)
	  val e = C.objects.head //Since the monoid has only one object, we can just get it -- the head of the list of objects. Equally valid: e = Box("an element")
	      (for (k <- 0 until 8) yield C.morphismsOfLength(k)(e, e).size) should equal (List(1,1,1,1,1,0,0,0)) //How many morphisms of length 0,1,...7?

  }
}