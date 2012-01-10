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
    val FCM = Examples.FiniteCyclicMonoid(7, 4)
    // Equality would fail because the morphisms get renamed by opposite: f => f^op, and f^op^op isn't the same as f.
    FCM.opposite.opposite should beIsomorphicTo(FCM)
  }

  "beIsomorphicTo" should "see the iso between saturated commutative triangle and Chain2" in { //TODO Is this test in the right place? Does it work? Do we have BeIsomorphicAsCategoriesTo, or any other such guy?
    val SaturatedCommutativeTriangle = {
      val ArrowsList = List(
        "V0" --- "E00" --> "V0",
        "V0" --- "E01" --> "V1",
        "V0" --- "E02" --> "V2",
        "V1" --- "E11" --> "V1",
        "V1" --- "E12" --> "V2",
        "V2" --- "E22" --> "V2")
      val RelationsId0 = List(
        ("V0" --- "E00" --> "V0" --- "E00" --> "V0") === ("V0" --- "E00" --> "V0"),
        ("V0" --- "E00" --> "V0" --- "E01" --> "V1") === ("V0" --- "E01" --> "V1"),
        ("V0" --- "E00" --> "V0" --- "E02" --> "V2") === ("V0" --- "E02" --> "V2"))
      val RelationsId1 = List(
        ("V0" --- "E01" --> "V1" --- "E11" --> "V1") === ("V0" --- "E01" --> "V1"),
        ("V1" --- "E11" --> "V1" --- "E11" --> "V1") === ("V1" --- "E11" --> "V1"),
        ("V1" --- "E11" --> "V1" --- "E12" --> "V2") === ("V1" --- "E12" --> "V2"))
      val RelationsId2 = List(
        ("V0" --- "E02" --> "V2" --- "E22" --> "V2") === ("V0" --- "E02" --> "V2"),
        ("V1" --- "E12" --> "V2" --- "E22" --> "V2") === ("V1" --- "E12" --> "V2"),
        ("V2" --- "E22" --> "V2" --- "E22" --> "V2") === ("V2" --- "E22" --> "V2"))
      val Comp012 = List(
        ("V0" --- "E01" --> "V1" --- "E12" --> "V2") === ("V0" --- "E02" --> "V2"))

      Ontology(
        objects = List("V0", "V1", "V2"),
        arrows = ArrowsList,
        relations = RelationsId0 ::: RelationsId1 ::: RelationsId2 ::: Comp012)
    }
    Examples.Chain(2) should beIsomorphicTo(SaturatedCommutativeTriangle)
  }

  

  //   "fullSubcategorySpannedBy" should "take Chains to Chains" in {//TODO Make a working fullSubcategorySpannedBy method.
  //	   val X = Examples.Chain(5)
  //	   val RHS = Examples.Chain(3)
  //	   val LHS = X.fullSubcategorySpannedBy(List("0","1","2","3")/*The fact that these are boxes should be inherent?*/)
  //	   LHS should equal(RHS)
  //   }

}