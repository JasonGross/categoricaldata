package net.metaphor.api

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.metaphor.examples.Examples
import net.metaphor.CustomMatchers

@RunWith(classOf[JUnitRunner])
class OntologyTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._

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

  "Dataset.isIsomorphicTo" should "be reflexive" in {
    for (dataset <- List(Drawers /* TODO more */ )) {
      dataset should beIsomorphicTo(dataset)
    }
  }
  
//  "Dataset.findIsomorphismsTo" should "count isomorphisms" in {
//    Drawers.findIsomorphismsTo(Drawers).size should equal(6)
//  }

}