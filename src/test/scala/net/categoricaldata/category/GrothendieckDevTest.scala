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
import net.categoricaldata.ontology.Box

@RunWith(classOf[JUnitRunner])
class GrothendieckDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

"grothendieck of dataset X" should "have left pushforward of terminal dataset isomorphic to X" in {
   val X=GraphDataset120114
   val pi: Translation = X.grothendieck
   val E=pi.source  
   val t=Examples.TerminalDataset(E)//I think Scott may have told me not to use this, but I can't recall or find what I should be using.
   val LHS=X
   val RHS=pi.__!(t)
   LHS should beIsomorphicTo(RHS)
  }


  
  val GraphDataset120114 = Dataset(source = Examples.Graph,
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
}