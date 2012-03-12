package net.categoricaldata.category

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.categoricaldata.ontology._
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
import net.categoricaldata.ontology._


@RunWith(classOf[JUnitRunner])
class ColimitDevTest extends FlatSpec with ShouldMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

  val C = Ontology(objects = List("article", "author", "authorship"), arrows = List("authorship" --- "by" --> "author", "authorship" --- "has title" --> "article"))
  val i = Dataset(C,
    onObjects = Map("article" -> List("X", "Y", "Z"), "author" -> List("a", "b", "c", "d"), "authorship" -> List("Xa", "Xb", "Yb", "Yc", "Zd")),
    onMorphisms = Map(("authorship" --- "by" --> "author") -> (_.drop(1)), ("authorship" --- "has title" --> "article") -> (_.take(1))))

  "colimit" should "correctly find two connected components of the authorship graph" in {
    val colimit = i.colimitSet

    colimit.toIterable.size should equal(2)
  }

 "colimitMorphism" should "do something" in {
	val F = Examples.ReverseGraph
	val G = Examples.GraphToFunction
	val H = Examples.GraphToFunction
    val A = F.source
    val B = G.source
    val C : G.target.type= G.target
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
    
   println(F.pullback.colimitMorphism(G.pullback(Drawers).asInstanceOf[F.target.FunctorToSet]))
 }      
  
 "colimitMorphism" should "do something else" in {
	val G = Examples.GraphToFunction
	val H = Examples.GraphToFunction
    val A = H.source
    val B = G.source
    val F : Translation = Ontologies.identity(A)
    val C : G.target.type= G.target
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
    
   println(F.pullback.colimitMorphism(G.pullback(Drawers).asInstanceOf[F.target.FunctorToSet]))
 }      
  
}

