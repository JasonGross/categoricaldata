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
class OntologyTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._

  def setOfSize(n: Int) = Dataset(Examples.Ord(0), onObjects = Map("V0" -> (0 until n).map(_.toString)), onMorphisms = Map())

  val Drawers = Dataset(Examples.Ord(1),
    onObjects = Map(
      "V0" -> List("Item 1", "Item 2", "Item 3", "Item 4"),
      "V1" -> List("Top Drawer", "Bottom Drawer")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "Item 1" -> "Top Drawer",
        "Item 2" -> "Bottom Drawer",
        "Item 3" -> "Top Drawer",
        "Item 4" -> "Top Drawer")))

  val Drawers2 = Dataset(Examples.Ord(1),
    onObjects = Map(
      "V0" -> List("Item 1", "Item 2", "Item 3", "Item 4"),
      "V1" -> List("Top Drawer", "Bottom Drawer")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "Item 1" -> "Top Drawer",
        "Item 2" -> "Bottom Drawer",
        "Item 3" -> "Bottom Drawer",
        "Item 4" -> "Top Drawer")))

  val DrawersRenamed = Dataset(Examples.Ord(1),
    onObjects = Map(
      "V0" -> List("Item 5", "Item 6", "Item 7", "Item 8"),
      "V1" -> List("Left Drawer", "Right Drawer")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "Item 5" -> "Left Drawer",
        "Item 6" -> "Right Drawer",
        "Item 7" -> "Left Drawer",
        "Item 8" -> "Left Drawer")))

  "Dataset.isIsomorphicTo" should "be reflexive" in {
    for (dataset <- List(Drawers /* TODO add some more? */ )) {
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

}