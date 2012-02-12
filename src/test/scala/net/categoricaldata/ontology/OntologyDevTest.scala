package net.categoricaldata.ontology

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
import net.tqft.toolkit.arithmetic.Factorial

@RunWith(classOf[JUnitRunner])
class OntologyDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

  "opposite" should "be an involution for FiniteCyclicMonoid" in {
    val FCM = Examples.FiniteCyclicMonoid(7, 4)
    // Equality would fail because the morphisms get renamed by opposite: f => f^op, and f^op^op isn't the same as f.
    FCM.opposite.opposite should beIsomorphicTo(FCM)
  }

  "beIsomorphicTo" should "see the iso between saturated commutative triangle and Chain2" in { //TODO Does it work? Do we have BeIsomorphicAsCategoriesTo, or any other such guy?
    Examples.Chain(2) should beIsomorphicTo(SaturatedCommutativeTriangle)
  }
  
  "fullSubcategory" should "take Indiscrete to Indiscrete" in {
    val X = Examples.IndiscreteCategory(7)
    val LHS = X.fullSubcategory("V1","V2","V3","V4")
    val RHS = Examples.IndiscreteCategory(4)
    LHS should beIsomorphicTo(RHS)
  }
  
  "fullSubcategory" should "take SaturatedCommutativeTriangle to SaturatedArrow" in {
    val X=SaturatedCommutativeTriangle
    val LHS=X.fullSubcategory("V0","V1")
    val RHS=SaturatedArrow
    LHS should beIsomorphicTo(RHS)
  }
  
  "fullSubcategory" should "take Chain7 to Chain3" in {
    val X=Examples.Chain(7)
    val LHS=X.fullSubcategory("V0","V1","V2","V3")
    val RHS=Examples.Chain(3)
    LHS should beIsomorphicTo(RHS)
  }

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
      relations = List.concat(RelationsId0, RelationsId1, RelationsId2, Comp012))
  }

val SaturatedArrow = {
    val ArrowsList = List(
      "V0" --- "E00" --> "V0",
      "V0" --- "E01" --> "V1",
      "V1" --- "E11" --> "V1")
    val RelationsId0 = List(
      ("V0" --- "E00" --> "V0" --- "E00" --> "V0") === ("V0" --- "E00" --> "V0"),
      ("V0" --- "E00" --> "V0" --- "E01" --> "V1") === ("V0" --- "E01" --> "V1"))
    val RelationsId1 = List(
      ("V0" --- "E01" --> "V1" --- "E11" --> "V1") === ("V0" --- "E01" --> "V1"),
      ("V1" --- "E11" --> "V1" --- "E11" --> "V1") === ("V1" --- "E11" --> "V1"))
  
    Ontology(
      objects = List("V0", "V1"),
      arrows = ArrowsList,
      relations = List.concat(RelationsId0, RelationsId1))
  }
}