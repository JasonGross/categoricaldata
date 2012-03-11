package net.categoricaldata.category

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
import net.categoricaldata.ontology._
import net.categoricaldata.ontology.Translation


/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class PullbackDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._
  
  
  "pullback" should "work" in {
    val C=Examples.Graph
    val D=Examples.Chain(1)
    val F=Examples.GraphToFunction
    val o:D.O = ???//"V0"
    val p:D.O = ???//"V1"
    val g:D.G = ???//"E01"
    val sg= ???//coslice(D.generatorAsMorphism(g))
    val Fg = ???//sg.functor// (F|o) --> (F|p)
    val Fs = ???//sg.source // (F|o) --> C
    val Ft = ???//sg.target // (F|p) --> C
    val cosliceo = ???//Fs.source
    val coslicep = ???//Ft.source
   
   // Fg.pullback(Ft.pullback(i).asInstanceOf[Fg.target.FunctorToSet]) should equal(Fs.pullback(i.asInstanceOf[Fs.target.FunctorToSet]))
  }

  
}
