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
class RightPushforwardDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._

  "__*" should "work nicely with the map from E2 to PointedSets" in {
    val FunkyE2Dataset = Dataset(
      source = Examples.E2,
      onObjects = Map(
        "0" -> List("a", "b", "c", "d"),
        "1" -> List("1", "2", "3")),
      onMorphisms = Map(
        "0" --- "E01" --> "1" -> Map(
          "a" -> "1",
          "b" -> "1",
          "c" -> "2",
          "d" -> "3"),
        "1" --- "E10" --> "0" -> Map(
          "1" -> "a",
          "2" -> "b",
          "3" -> "d")))

    val E2ToPointedSetsRPushFunky = Dataset(
      source = Examples.PointedSets,
      onObjects = Map(
        "an element" -> List("a1a", "b1a", "d3d"),
        "a pointed set" -> List("a1", "d3")),
      onMorphisms = Map(
        ("an element" --- "is in" --> "a pointed set") -> Map(
          "a1a" -> "a1",
          "ba1" -> "a1",
          "d3d" -> "d3"),
        ("a pointed set" --- "has as chosen" --> "an element") -> Map(
          "a1" -> "a1a",
          "d3" -> "d3d")))

    Examples.E2ToPointedSets.__*(FunkyE2Dataset) should beIsomorphicTo(E2ToPointedSetsRPushFunky)
  }

  
  // This one is failing because isomorphism testing is broken (not even reflexive!)
  "__*" should "reverse graph as expected" in {

    val DavidsFunkyGraph = Dataset(source = Examples.Grph,
      onObjects = Map(
        "an edge" -> List("f", "g", "h", "i", "j"),
        "a vertex" -> List("A", "B", "C", "D")),
      onMorphisms = Map(
        ("an edge" --- "has as source" --> "a vertex") -> Map(
          "f" -> "A",
          "g" -> "A",
          "h" -> "B",
          "i" -> "A",
          "j" -> "C"),
        ("an edge" --- "has as target" --> "a vertex") -> Map(
          "f" -> "B",
          "g" -> "B",
          "h" -> "C",
          "i" -> "C",
          "j" -> "C")))

    val DavidsFunkyGraphReversed = Dataset(source = Examples.Grph,
      onObjects = Map(
        "an edge" -> List("f", "g", "h", "i", "j"),
        "a vertex" -> List("A", "B", "C", "D")),
      onMorphisms = Map(
        ("an edge" --- "has as source" --> "a vertex") -> Map(
          "f" -> "B",
          "g" -> "B",
          "h" -> "C",
          "i" -> "C",
          "j" -> "C"),
        ("an edge" --- "has as target" --> "a vertex") -> Map(
          "f" -> "A",
          "g" -> "A",
          "h" -> "B",
          "i" -> "A",
          "j" -> "C")))

    println
    println("Output from \"__* should reverse graph as expected\":")

    val X = DavidsFunkyGraph
    val LHS = DavidsFunkyGraphReversed
    val RHS = Examples.ReverseGraph.__*(DavidsFunkyGraph)
    println("Original graph: " + X)
    println("Reversed graph: " + LHS)
    println("Right pushforward of original: " + RHS)
    
    LHS should beIsomorphicTo(RHS)
  }
  
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

          
    // Scott: I think this is the wrong test, per my email 2012-01-09.
          
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