package net.categoricaldata.category

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.categoricaldata.ontology._
import net.categoricaldata.util.CustomMatchers
import net.tqft.toolkit.arithmetic.Factorial

@RunWith(classOf[JUnitRunner])
class LeftPushforwardTest extends FlatSpec with ShouldMatchers with CustomMatchers {
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

  val DavidsFunkySet2 = Dataset(source = Examples.Chain(0),
    onObjects = Map(
      "V0" -> List("1978", "Scott's birthyear", "1868", "1861")),
    onMorphisms = Map())

  "__!" should "work with terminal functor on 'function'" in {
    val shriek = Ontologies.morphismToTerminalObject(Examples.Chain(1)).__!(DavidsFunkyFunction)
    shriek should beIsomorphicTo(DavidsFunkySet2)
  }

  "__!" should "preserve the initial dataset" in {
    val FCM = Examples.FiniteCyclicMonoid(10, 7)
    val F1 = Ontologies.terminalObject.findAllTranslationsTo(FCM).head
    for (F <- List(F1)) {
      F.__!(F.source.Datasets.initialObject) should beIsomorphicTo((F.target.Datasets.initialObject))
    }
  }

  "__!" should "work reverse graph as expected" in {
    val DavidsFunkyGraph = Dataset(source = Examples.Graph,
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

    val DavidsFunkyGraphReversed = Dataset(source = Examples.Graph,
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

    val LHS = Examples.ReverseGraph.__!(DavidsFunkyGraph)
    val RHS = DavidsFunkyGraphReversed
    LHS should beIsomorphicTo(RHS)
  }

  val DavidsFunkyFiniteCyclicMonoid = Dataset(
    source = Examples.FiniteCyclicMonoid(2, 1),
    onObjects = Map("an element" -> List("David", "Scott", "UC Berkeley", "MIT", "succDavid", "succScott", "succUC Berkeley", "succMIT")),
    onMorphisms = Map("an element" --- "has as successor" --> "an element" -> Map(
      "David" -> "succDavid",
      "Scott" -> "succScott",
      "UC Berkeley" -> "succUC Berkeley",
      "MIT" -> "succMIT",
      "succDavid" -> "succDavid",
      "succScott" -> "succScott",
      "succUC Berkeley" -> "succUC Berkeley",
      "succMIT" -> "succMIT")))

  val DavidsFunkySet1 = Dataset(source = Examples.Chain(0),
    onObjects = Map(
      "V0" -> List("David", "Scott", "UC Berkeley", "MIT")),
    onMorphisms = Map())

  "__!" should "properly convert a set to a step-and-hold FCM2_1" in {
    val F = Ontologies.terminalObject.findAllTranslationsTo(Examples.FiniteCyclicMonoid(2, 1)).head
    F.__!(DavidsFunkySet1) should beIsomorphicTo(DavidsFunkyFiniteCyclicMonoid)
  }

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
  val ThreeElementsIso = Dataset(
    source = Examples.Isomorphism,
    onObjects = Map(
      "0" -> List("a", "b", "c"),
      "1" -> List("a", "b", "c")),
    onMorphisms = Map(
      ("0" --- "E01" --> "1") -> Map(
        "a" -> "a",
        "b" -> "b",
        "c" -> "c"),
      ("1" --- "E10" --> "0") -> Map(
        "a" -> "a",
        "b" -> "b",
        "c" -> "c")))

  "__!" should "work with PointedSetsToIsomorphism" in {
    val X = OneTwoThreePointed
    val LHS = ThreeElementsIso
    val RHS = Examples.PointedSetsToIsomorphism.__!(X)
    LHS should beIsomorphicTo(RHS)
  }

  val FCM4_3 = Dataset(source = Examples.FiniteCyclicMonoid(4, 3),
    onObjects = Map(
      "an element" -> List("a", "b")),
    onMorphisms = Map(
      "an element" --- "has as successor" --> "an element" -> Map(
        "a" -> "b",
        "b" -> "b")))

  val FCM4_3Times2LToFCM5_3 = Dataset(source = Examples.FiniteCyclicMonoid(5, 3),
    onObjects = Map(
      "an element" -> List("a1", "b1", "a2", "b2")),
    onMorphisms = Map(
      "an element" --- "has as successor" --> "an element" -> Map(
        "a1" -> "a2",
        "b1" -> "b2",
        "a2" -> "b1",
        "b2" -> "b1")))

  "__!" should "provide a 'half-speed' FCM-thing" in {
    val X = FCM4_3
    val LHS = Examples.TranslationFiniteCyclicMonoids(4, 3, 5, 3, 2).__!(X)
    val RHS = FCM4_3Times2LToFCM5_3
    LHS should beIsomorphicTo(RHS)
  }

}