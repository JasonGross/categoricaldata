package net.categoricaldata.ontology

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.tqft.toolkit.arithmetic.Factorial
import net.categoricaldata.util.CustomMatchers

@RunWith(classOf[JUnitRunner])
class DatasetDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

}