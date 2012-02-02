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

}