package net.metaphor.api

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ColimitTest extends FlatSpec with ShouldMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._

  val C = ontology(objects = List("article", "author", "authorship"), arrows = List("authorship" --- "by" --> "author", "authorship" --- "has title" --> "article"))
  val i = dataset(C,
    onObjects = Map("article" -> List("X", "Y", "Z"), "author" -> List("a", "b", "c", "d"), "authorship" -> List("Xa", "Xb", "Yb", "Yc", "Zd")),
    onMorphisms = Map(("authorship" --- "by" --> "author") -> (_.drop(1)), ("authorship" --- "has title" --> "article") -> (_.take(1))))

  "colimit" should "correctly find two connected components of the authorship graph (1)" in {
    val colimit = i.colimit(C.adjoinTerminalObject.terminalObject)
    // or just
    //     val colimit = i.colimit(Box("*"))

    for (s <- colimit.toIterable) println(s)
    colimit.toIterable.size should equal(2)
  }

  "colimit" should "correctly find two connected components of the authorship graph (2)" in {
    val colimit = i.colimitSet

    for (s <- colimit.toIterable) println(s)
    colimit.toIterable.size should equal(2)
  }

}

