package net.metaphor.api

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.metaphor.examples.Examples
import net.metaphor.CustomMatchers
import net.tqft.toolkit.arithmetic.Factorial

@RunWith(classOf[JUnitRunner])
class OntologyTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._

  "Isomorphism.morphismsOfLength" should "have the right sizes" in {
    import net.tqft.toolkit.arithmetic.Mod._

    val I = Examples.Isomorphism
    val List(o0, o1) = I.objects
    for (k <- 0 until 10) I.wordsOfLength(k)(o0, o1) should have size (k mod 2)
    for (k <- 0 until 10) I.wordsOfLength(k)(o0, o0) should have size (k - 1 mod 2)

    val List(m01) = I.generators(o0, o1).map(I.generatorAsMorphism(_))
    val List(m10) = I.generators(o1, o0).map(I.generatorAsMorphism(_))

    I.identity(o0) should equal(I.compose(m01, m10))
    
    for (k <- 1 until 10) I.morphismsOfLength(k)(o0, o0) should be('empty)
  }

}