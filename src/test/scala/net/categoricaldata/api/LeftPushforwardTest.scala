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
    val shriek = Examples.TerminalFunctor(Examples.Chain(1)).__!(DavidsFunkyFunction)
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


  // TODO (David) this one is working; give it a good name and move to LeftPushforwardTest
  
  "__!" should "properly convert a set to a step-and-hold FCM2_1" in {
    val F = Ontologies.terminalObject.findAllTranslationsTo(Examples.FiniteCyclicMonoid(2, 1)).head
    F.__!(DavidsFunkySet1) should beIsomorphicTo(DavidsFunkyFiniteCyclicMonoid)
  }

}