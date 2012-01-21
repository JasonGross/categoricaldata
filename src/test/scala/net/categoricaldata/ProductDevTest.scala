package net.categoricaldata.sets

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.ontology._
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.permutations.Permutations
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers

@RunWith(classOf[JUnitRunner])
class ProductDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

  "Product of sets" should "correctly multiply 3 by 2" in {
    Sets.product(List("1", "2", "3"), List("a","b")) should have size(6)
    Sets.product(List("1", "2", "3"), List("2","3")) should have size(6)
  }

  "Coproduct of sets" should "correctly add 3 and 2" in {
    Sets.coproduct(List("1", "2", "3"), List("a", "b")) should have size(5)
    Sets.coproduct(List("1", "2", "3"), List("1", "2")) should have size(5)
  }

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
    //TODO (Scott) this doesn't compile; because beIsomorphicTo doesn't do FSets
    //    LHS should beIsomorphicTo(RHS)
  }

  "Coproduct of datasets" should "be taken pointwise on PointedSets" in {
    val PS: Examples.PointedSets.type = Examples.PointedSets
    val X = OneTwoThreePointed
    val XX = PS.Datasets.coproduct(X, X)
    val E = X(Box("an element"))
    val EE = Sets.coproduct(E, E)
    val LHS = XX(Box("an element"))
    val RHS = EE
    //TODO (Scott) this doesn't compile; because beIsomorphicTo doesn't do FSets
    //    LHS should beIsomorphicTo(RHS)
  }

}