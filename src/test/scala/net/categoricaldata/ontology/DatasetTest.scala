package net.categoricaldata.ontology

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.tqft.toolkit.arithmetic.Factorial
import net.categoricaldata.util.CustomMatchers
import net.categoricaldata.sets.Sets

@RunWith(classOf[JUnitRunner])
class DatasetTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

  def setOfSize(n: Int) = Dataset(Examples.Chain(0), onObjects = Map("V0" -> (0 until n).map(_.toString)), onMorphisms = Map())

  val Drawers = Dataset(Examples.Chain(1),
    onObjects = Map(
      "V0" -> List("Item 1", "Item 2", "Item 3", "Item 4"),
      "V1" -> List("Top Drawer", "Bottom Drawer")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "Item 1" -> "Top Drawer",
        "Item 2" -> "Bottom Drawer",
        "Item 3" -> "Top Drawer",
        "Item 4" -> "Top Drawer")))

  val Drawers2 = Dataset(Examples.Chain(1),
    onObjects = Map(
      "V0" -> List("Item 1", "Item 2", "Item 3", "Item 4"),
      "V1" -> List("Top Drawer", "Bottom Drawer")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "Item 1" -> "Top Drawer",
        "Item 2" -> "Bottom Drawer",
        "Item 3" -> "Bottom Drawer",
        "Item 4" -> "Top Drawer")))

  val DrawersRenamed = Dataset(Examples.Chain(1),
    onObjects = Map(
      "V0" -> List("Item 5", "Item 6", "Item 7", "Item 8"),
      "V1" -> List("Left Drawer", "Right Drawer")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "Item 5" -> "Left Drawer",
        "Item 6" -> "Right Drawer",
        "Item 7" -> "Left Drawer",
        "Item 8" -> "Left Drawer")))

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

  "Dataset.isIsomorphicTo" should "be reflexive" in {
    for (dataset <- List(Drawers, Examples.ReverseGraph.__*(DavidsFunkyGraph))) {
      dataset should beIsomorphicTo(dataset)
    }
  }
  "Dataset.isIsomorphicTo" should "not care about the names of objects" in {
    Drawers should beIsomorphicTo(DrawersRenamed)
  }

  "Dataset.isIsomorphicTo" should "not have false positives" in {
    Drawers should not(beIsomorphicTo(Drawers2))
  }

  "Dataset.isIsomorphicTo" should "return quickly, even when there are 100! isomorphisms" in {
    val n = 100
    setOfSize(n) should beIsomorphicTo(setOfSize(n))
  }

  "Dataset.findIsomorphismsTo" should "count automorphisms of finite sets" in {
    for (n <- 0 to 3) setOfSize(n).findIsomorphismsTo(setOfSize(n)).size should equal(Factorial(n))
  }

  "Dataset.findIsomorphismsTo" should "return an initial segment of automorphisms, even when there are 100!" in {
    setOfSize(100).findIsomorphismsTo(setOfSize(100)).take(5) should have size (5)
  }

  "Dataset.findIsomorphismsTo" should "count automorphisms of Drawers" in {
    Drawers.findIsomorphismsTo(Drawers).size should equal(6)
  }

  "Dataset.isIsomorphicTo" should "find that even a complex dataset is isomorphic to itself" in {
    /*
     *  FIXME Scott to David:
     *  Test is a class, not an object, so you can't refer to its members (e.g. DavidsFunkyGraph) without creating an instance.
     *  Either write new Test.DavidsFunkyGraph, or, better, move DavidsFunkyGraph to a companion object.
     *  You'll also need to import Test, as it lives in a different package.
     */
    //    Examples.ReverseGraph.__*(Test.DavidsFunkyGraph) should beIsomorphicTo(Examples.ReverseGraph.__*(Test.DavidsFunkyGraph))
  }

  "dataset" should "throw an exception if it does not conform to relations in the source" in {
    lazy val badData = Dataset(source = Examples.Isomorphism,
      onObjects = Map(
        "0" -> List("x", "y"),
        "1" -> List("z")),
      onMorphisms = Map(
        "0" --- "E01" --> "1" -> Map(
          "x" -> "z",
          "y" -> "z"),
        "1" --- "E10" --> "0" -> Map(
          "z" -> "x")))
    evaluating { badData } should produce[IllegalArgumentException]
  }

  {
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

    "Product of datasets" should "be taken pointwise on PointedSets" in {
      val PS: Examples.PointedSets.type = Examples.PointedSets
      val X = OneTwoThreePointed
      val XX = PS.Datasets.product(X, X)
      val E = X(Box("an element"))
      val EE = Sets.product(E, E)
      val LHS = XX(Box("an element"))
      val RHS = EE
      LHS should have size (RHS.size)
    }

    "Coproduct of datasets" should "be taken pointwise on PointedSets" in {
      val PS: Examples.PointedSets.type = Examples.PointedSets
      val X = OneTwoThreePointed
      val XX = PS.Datasets.coproduct(X, X)
      val E = X(Box("an element"))
      val EE = Sets.coproduct(E, E)
      val LHS = XX(Box("an element"))
      val RHS = EE
      LHS should have size (RHS.size)
    }
  }
}