package net.metaphor.api

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.metaphor.examples.Examples
import net.tqft.toolkit.arithmetic.Factorial
import net.metaphor.util.CustomMatchers

@RunWith(classOf[JUnitRunner])
class DatasetDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._

  "Dataset.isIsomorphicTo" should "be reflexive" in {
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
    
    
    for (dataset <- List(Examples.ReverseGraph.__*(DavidsFunkyGraph))) {
      println(dataset)
      // this works
      dataset should equal(dataset)
      // but this doesn't
      dataset should beIsomorphicTo(dataset)
    }
  }

}