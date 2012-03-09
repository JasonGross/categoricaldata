package net.categoricaldata.category

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
import net.categoricaldata.ontology._

/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class PullbackDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._
  
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

  "pullback" should "turn commutative diagrams into isomorphisms" in {
	val F = Examples.ReverseGraph
	val G = Examples.GraphToFunction
	val H = Examples.GraphToFunction
    val A = F.source
    val B = G.source
    val C = G.target
    require(C == H.target)
    require(A == H.source)
    require(B == F.target)
    val Drawers = Dataset(C,
    onObjects = Map(
      "V0" -> List("Item 1", "Item 2", "Item 3", "Item 4"),
      "V1" -> List("Top Drawer", "Bottom Drawer")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "Item 1" -> "Top Drawer",
        "Item 2" -> "Bottom Drawer",
        "Item 3" -> "Top Drawer",
        "Item 4" -> "Top Drawer")))
    
  //  F.pullback(G.pullback(Drawers).asInstanceOf[F.target.FunctorToSet]) should beIsomorphicTo(H.pullback(Drawers))
  }
  
  
}
