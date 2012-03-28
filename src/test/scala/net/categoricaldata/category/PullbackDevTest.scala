package net.categoricaldata.category

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
import net.categoricaldata.ontology._
import net.categoricaldata.ontology.Translation


/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class PullbackDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._
  
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

  
  "equality testing for datasets" should "yield true when the respective toStrings are equal" in {
    val C=Examples.Graph
    val F=Examples.GraphToFunction
    val D:F.target.type = F.target //  Examples.Chain(1)
    val o:D.O = Box("V0")
    val p:D.O = Box("V1")
    val g:D.G = Arrow(o,p,"E01")
    val sg= F.coslice(D.generatorAsMorphism(g))
    val Fg = sg.functor// (F|o) --> (F|p)
    val Fs = sg.source // (F|o) --> C
    val Ft = sg.target // (F|p) --> C
    val cosliceo = Fs.source
    val coslicep = Ft.source
    val LHS = Fg.pullback(Ft.pullback(DavidsFunkyGraph.asInstanceOf[Ft.target.FunctorToSet]).asInstanceOf[Fg.target.FunctorToSet])
    val RHS = Fs.pullback(DavidsFunkyGraph.asInstanceOf[Fs.target.FunctorToSet])
    val LHStoStringHash = LHS.toString.hashCode
    val RHStoStringHash = RHS.toString.hashCode
    if (LHStoStringHash == RHStoStringHash) LHS should equal(RHS)
  }

  
}
