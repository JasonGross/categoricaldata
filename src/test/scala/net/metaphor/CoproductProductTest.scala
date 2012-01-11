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
}