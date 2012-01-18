package net.categoricaldata.category

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
import net.tqft.toolkit.arithmetic.Factorial

@RunWith(classOf[JUnitRunner])
class RightPushforwardDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

  // Scott: This is the wrong test, per my email 2012-01-09.
  // FIXME (David) 
  "__* along PointedSetsToIsomorphism" should "take a retraction and return two sets isomorphic to its base." in {

    val OneTwoThreePointed = Dataset(
      source = Examples.PointedSets,
      onObjects = Map(
        "an element" -> List("a1", "b1", "b2", "c1", "c2", "c3"),
        "a pointed set" -> List("a", "b", "c")),
      onMorphisms = Map(
        ("an element" --- "is in" --> "a pointed set") -> Map(
          "a1" -> "a",
          "b1" -> "b",
          "b2" -> "b",
          "c1" -> "c",
          "c2" -> "c",
          "c3" -> "c"),
        ("a pointed set" --- "has as chosen" --> "an element") -> Map(
          "a" -> "a1",
          "b" -> "b1",
          "c" -> "c1")))

    val SixElementsIso = Dataset(
      source = Examples.Isomorphism,
      onObjects = Map(
        "0" -> List("a1", "b1", "b2", "c1", "c2", "c3"),
        "1" -> List("a1", "b1", "b2", "c1", "c2", "c3")),
      onMorphisms = Map(
        ("0" --- "E01" --> "1") -> Map(
          "a1" -> "a1",
          "b1" -> "b1",
          "b2" -> "b2",
          "c1" -> "c1",
          "c2" -> "c2",
          "c3" -> "c3"),
        ("1" --- "E10" --> "0") -> Map(
          "a1" -> "a1",
          "b1" -> "b1",
          "b2" -> "b2",
          "c1" -> "c1",
          "c2" -> "c2",
          "c3" -> "c3")))

    println
    println("Output from \"__* along PointedSetsToIsomorphism should take a retraction and return two sets isomorphic to its base.\":")
    println
    val X = OneTwoThreePointed
    val LHS = SixElementsIso
    val RHS = Examples.PointedSetsToIsomorphism.__*(X)
    println("Original retraction: "); println(X); println
    println("Expected isomorphism: "); println(LHS); println
    println("Right pushforward of original retraction: "); println(RHS)
    LHS should beIsomorphicTo(RHS)
  }

}