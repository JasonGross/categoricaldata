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

  val table1 = 
"""
*,*
a,b
b,a
c,c
"""
val table2=
"""
*,*
zz,zz
xx,yy
yy,xx
"""   
    
  "CSVLoader" should "work!" in {
    val d1 = CSVLoader.fromStrings(table1).toDataset
    val d2 = CSVLoader.fromStrings(table2).toDataset
    d1 should beIsomorphicTo(d2)
  }
}