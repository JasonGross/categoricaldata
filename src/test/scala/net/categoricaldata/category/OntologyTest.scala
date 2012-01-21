package net.categoricaldata.ontology

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
import net.tqft.toolkit.arithmetic.Factorial

@RunWith(classOf[JUnitRunner])
class OntologyTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

  "Isomorphism.morphismsOfLength" should "have the right sizes" in {
    import net.tqft.toolkit.arithmetic.Mod._

    val I = Examples.Isomorphism
    val List(o0, o1) = I.objects
    for (k <- 0 until 10) I.wordsOfLength(k)(o0, o1) should have size (k mod 2)
    for (k <- 0 until 10) I.wordsOfLength(k)(o0, o0) should have size (k - 1 mod 2)

    I.relations(o0, o0) should have size (1)

    val List(m01) = I.generators(o0, o1).map(I.generatorAsMorphism(_))
    val List(m10) = I.generators(o1, o0).map(I.generatorAsMorphism(_))

    I.identity(o0) should equal(I.compose(m01, m10))

    I.compose(m01, m10, m01, m10) should equal(I.identity(o0))

    (for (k <- 0 until 5) yield I.morphismsOfLength(k)(o0, o0).size) should equal(List(1, 0, 0, 0, 0))
  }

  {
    val C = Examples.FiniteCyclicMonoid(5, 3)
    val e = C.objects.head //Since the monoid has only one object, we can just get it -- the head of the list of objects. Equally valid: e = Box("an element")
    val g = C.generatorAsMorphism(C.generators(e, e).head)

    "FiniteCyclicMonoid.compose" should "reflect the relations correctly" in {
      C.power(g, 5) should equal(C.power(g, 3))
      C.power(g, 6) should equal(C.power(g, 4))
      C.power(g, 10) should equal(C.power(g, 4))
    }

    "FiniteCyclicMonoid.morphismsOfLength" should "have the right sizes" in {
      (for (k <- 0 until 8) yield C.morphismsOfLength(k)(e, e).size) should equal(List(1, 1, 1, 1, 1, 0, 0, 0)) //How many morphisms of length 0,1,...7?
    }
  }

  "fullSubcategory" should "take Chains to Chains" in {
    val X = Examples.Chain(5)
    val LHS = X.fullSubcategory("V0", "V1", "V2", "V3")
    val RHS = Examples.Chain(3)
    LHS should equal(RHS)
  }

}