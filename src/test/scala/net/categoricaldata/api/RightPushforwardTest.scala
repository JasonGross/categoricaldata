package net.categoricaldata.api

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
import net.tqft.toolkit.arithmetic.Factorial

@RunWith(classOf[JUnitRunner])
class RightPushforwardTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

  val DavidsFunkyFunction = Dataset(source = Examples.Chain(1),
    onObjects = Map(
      "V0" -> List("David", "Scott", "UC Berkeley", "MIT"),
      "V1" -> List("1978", "Scott's birthyear", "1868", "1861")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "David" -> "1978",
        "Scott" -> "Scott's birthyear",
        "UC Berkeley" -> "1868",
        "MIT" -> "1861")))

  val DavidsFunkySet1 = Dataset(source = Examples.Chain(0),
    onObjects = Map(
      "V0" -> List("David", "Scott", "UC Berkeley", "MIT")),
    onMorphisms = Map())

  "__*" should "work with terminal functor on 'function'" in {
    val pushforward = Examples.TerminalFunctor(Examples.Chain(1)).__*(DavidsFunkyFunction)
    pushforward.isIsomorphicTo(DavidsFunkySet1) should equal(true)
  }

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

    val X = DavidsFunkyGraph
    val LHS = DavidsFunkyGraphReversed
    val RHS = Examples.ReverseGraph.__*(DavidsFunkyGraph)

    LHS should beIsomorphicTo(RHS)
  }

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
          "b1a" -> "a1",
          "d3d" -> "d3"),
        ("a pointed set" --- "has as chosen" --> "an element") -> Map(
          "a1" -> "a1a",
          "d3" -> "d3d")))

    Examples.E2ToPointedSets.__*(FunkyE2Dataset) should beIsomorphicTo(E2ToPointedSetsRPushFunky)
  }

  "__*" should "preserve the terminal dataset" in {
    val FCM = Examples.FiniteCyclicMonoid(10, 7)
    val F1 = Ontologies.terminalObject.findAllTranslationsTo(FCM).head
    for (F <- List(F1)) {
      F.__*(F.source.Datasets.terminalObject) should beIsomorphicTo((F.target.Datasets.terminalObject))
    }
  }

  "__* along Chain1ToIsomorphism" should "take a function and return two sets isomorphic to its target." in {

    val OneTwoThree = Dataset(
      source = Examples.Chain(1),
      onObjects = Map(
        "V0" -> List("a1", "b1", "b2", "c1", "c2", "c3"),
        "V1" -> List("a", "b", "c")),
      onMorphisms = Map(
        ("V0" --- "E01" --> "V1") -> Map(
          "a1" -> "a",
          "b1" -> "b",
          "b2" -> "b",
          "c1" -> "c",
          "c2" -> "c",
          "c3" -> "c")))

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

    val X = OneTwoThree
    val LHS = SixElementsIso
    val RHS = Examples.Chain1ToIsomorphism.__*(X)
    LHS should beIsomorphicTo(RHS)

  }

}