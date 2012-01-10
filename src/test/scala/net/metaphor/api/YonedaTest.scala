package net.metaphor.api

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.metaphor.examples.Examples
import net.metaphor.util.CustomMatchers
import net.tqft.toolkit.arithmetic.Factorial

@RunWith(classOf[JUnitRunner])
class YonedaTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._
    
  "yoneda" should "give the right answer for V0 in Chain2" in {
    val LHS = Examples.Chain(2).yoneda(Box("V0"))
    val RHS = Examples.Chain(2).Datasets.terminalObject
    LHS should beIsomorphicTo(RHS)  
  }


}