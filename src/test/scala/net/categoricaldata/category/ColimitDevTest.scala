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
	val G = Examples.ReverseGraph
    val A = F.source
    val B = G.source
    val C : G.target.type= G.target
    val H = Ontologies.identity(A)
    require(C == H.target)
    require(A == H.source)
    require(B == F.target)
    val FunkyGraph = Dataset(source = Examples.Graph,
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
        "j" -> "C")
      )
    )
        
    
   println("colimit on G", G.pullback(FunkyGraph.asInstanceOf[G.target.FunctorToSet]).colimitSet)
   println("colimit on H", H.pullback(FunkyGraph.asInstanceOf[H.target.FunctorToSet]).colimitSet)
   println()
   println(F.pullback.colimitMorphism(G.pullback(FunkyGraph.asInstanceOf[G.target.FunctorToSet]).asInstanceOf[F.target.FunctorToSet]))
 }      
  
       
  
}

