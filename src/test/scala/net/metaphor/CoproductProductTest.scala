package net.metaphor

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.metaphor.api.Ontology
import net.metaphor.api.Sets
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.permutations.Permutations
import net.metaphor.api.FiniteMorphisms
import net.metaphor.api.Ontologies
import net.metaphor.examples.Examples
import net.metaphor.util.CustomMatchers
/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class CoproductProductTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._
  
//  "Product of sets" should "correctly multiply 3 by 2" in { //TODO Scala isn't finding FSet. I tried importing Sets.scala and everything it imports, but to no avail.
//    val Three: FSet = List ("1","2", "3")
//    val Two: FSet = List ("a","b")
//    val LHS = Sets.product(Three,Two)
//    val RHS = List("1a","1b","2a","2b","3a","3b")
//    LHS should beIsomorphicTo(RHS)
//    
//  }
  
//  "Coproduct of sets" should "correctly add 3 and 2" in 
//    val Three: FSet = List ("1","2", "3")
//    val Two: FSet = List ("a","b")
//    val LHS = Sets.coproduct(Three,Two)
//    val RHS = List("1","2","3","a","b")
//    LHS should beIsomorphicTo(RHS)
//    
//  }
  
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

  
//  "Product of datasets" should "be taken pointwise on PointedSets" in { //TODO this doesn't compile, probably because my use of onObjects.
//    val PS = Examples.PointedSets
//    val X=OneTwoThreePointed
//    val XX = PS.Datasets.product(X,X)
//    val E =  X.onObjects("an element")
//    val EE = Sets.product(E,E)
//    val LHS=XX.onObjects("an element")
//    val RHS=EE
//    LHS should beIsomorphicTo(RHS)
//  }
//  
//  "Coproduct of datasets" should "be taken pointwise on PointedSets" in { //TODO this doesn't compile, probably because my use of onObjects.
//    val PS = Examples.PointedSets
//    val X=OneTwoThreePointed
//    val XX = PS.Datasets.coproduct(X,X)
//    val E =  X.onObjects("an element")
//    val EE = Sets.coproduct(E,E)
//    val LHS=XX.onObjects("an element")
//    val RHS=EE
//    LHS should beIsomorphicTo(RHS)
//  }
//  
}