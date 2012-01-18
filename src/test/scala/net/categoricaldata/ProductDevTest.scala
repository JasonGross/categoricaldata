package net.categoricaldata

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.ontology.Box
import net.categoricaldata.ontology.Ontology
import net.categoricaldata.ontology.Ontologies
import net.categoricaldata.sets.Sets
import net.categoricaldata.sets.FSet
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.permutations.Permutations
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class ProductDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

  "Product of sets" should "correctly multiply 3 by 2" in {
    val Three: FSet = List("1", "2", "3")
    val Two: FSet = List("a","b")
    val LHS = Sets.product(Three, Two)
    val RHS = List("1a", "1b", "2a", "2b", "3a", "3b")
    // TODO (Scott) this doesn't compile; because beIsomorphicTo doesn't do FSets
    //    LHS should beIsomorphicTo(RHS)
  }

  "Coproduct of sets" should "correctly add 3 and 2" in {
    val Three: FSet = List("1", "2", "3")
    val Two: FSet = List("a", "b")
    val LHS = Sets.coproduct(Three, Two)
    val RHS = List("1", "2", "3", "a", "b")
    // TODO (Scott) this doesn't compile; because beIsomorphicTo doesn't do FSets
    //    LHS should beIsomorphicTo(RHS)
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