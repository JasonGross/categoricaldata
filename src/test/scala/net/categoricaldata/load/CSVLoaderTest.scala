package net.categoricaldata.load

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.tqft.toolkit.arithmetic.Factorial
import net.categoricaldata.util.CustomMatchers

@RunWith(classOf[JUnitRunner])
class CSVLoaderTest extends FlatSpec with ShouldMatchers with CustomMatchers {

  "CSVLoader" should "work!" in {
    val d = CSVLoader.fromStrings(
"""
*,*
a,b
b,a
"""
    ).toDataset
    
    println(d)
  }
}