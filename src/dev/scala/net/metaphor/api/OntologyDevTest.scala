package net.metaphor.api

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.metaphor.examples.Examples
import net.metaphor.util.CustomMatchers
import net.tqft.toolkit.arithmetic.Factorial

@RunWith(classOf[JUnitRunner])
class OntologyDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._

   "opposite" should "be an involution for FiniteCyclicMonoid" in {
     val FCM = Examples.FiniteCyclicMonoid(7,4)
     // Equality would fail because the morphisms get renamed by opposite: f => f^op, and f^op^op isn't the same as f.
     FCM.opposite.opposite should beIsomorphicTo(FCM)
   }
  
 
   
//   "yoneda" should "exist for objects in FiniteCyclicMonoid" in { //TODO Activate this test when Yoneda has been made.
//     val FCM = Examples.FiniteCyclicMonoid(7,4)
//     FCM.Yoneda(box("V2")) should equal(FCM.Yoneda(box("V2")))
//   }
   
//   "fullSubcategorySpannedBy" should "take Chains to Chains" in {//TODO Make a working fullSubcategorySpannedBy method.
//	   val X = Examples.Chain(5)
//	   val RHS = Examples.Chain(3)
//	   val LHS = X.fullSubcategorySpannedBy(List("0","1","2","3")/*The fact that these are boxes should be inherent?*/)
//	   LHS should equal(RHS)
//   }

  
}