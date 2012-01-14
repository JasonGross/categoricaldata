package net.categoricaldata.api

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
import net.tqft.toolkit.arithmetic.Factorial

@RunWith(classOf[JUnitRunner])
class YonedaTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._
    
  "yoneda of V0" should "give the terminal dataset in Chain2" in {
    val LHS = Examples.Chain(2).yoneda(Box("V0"))
    val RHS = Examples.Chain(2).Datasets.terminalObject
    LHS should beIsomorphicTo(RHS)  
  }
  
//  "yoneda of V1" should "assign the emptyset to V0" in {//TODO this doesn't compile, probably because my use of onObjects.
//    val X:Dataset = Examples.Chain(2).yoneda(Box("V1"))
//    val LHS = X.onObjects(Box("V0"))
//    val RHS = Sets.initialObject
//    LHS should beIsomorphicTo(RHS)  
//  }

  "yoneda" should "give the terminal dataset for Isomorphism" in {//This tests that yoneda understands relations.
    val LHS = Examples.Isomorphism.yoneda(Box("0"))
    LHS.verifyRelations
    val RHS = Examples.Isomorphism.Datasets.terminalObject
    LHS should beIsomorphicTo(RHS)
  }
  
}