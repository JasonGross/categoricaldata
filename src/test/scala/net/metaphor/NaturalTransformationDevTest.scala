package net.metaphor

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.metaphor.api.Ontology
import net.metaphor.api.FiniteMorphisms
import net.metaphor.api.Ontologies
import net.metaphor.examples.Examples
import net.metaphor.util.CustomMatchers
import net.metaphor.api.Translation
/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class NaturalTransformationDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._

  "leftUnit" should "be an isomorphism for translations that are isomorphisms" in {
    val C=Examples.Grph
    val D=Examples.Grph
    val F=Examples.ReverseGraph
    IsNaturalIsomorphism(F.leftUnit) should equal(True)
  }
  
  "leftCounit" should "be an isomorphism for translations that are isomorphisms" in {
    val C=Examples.Grph
    val D=Examples.Grph
    val F=Examples.ReverseGraph
    IsNaturalIsomorphism(F.leftCounit) should equal(True)
  }
  
  "rightUnit" should "be an isomorphism for translations that are isomorphisms" in {
    val C=Examples.Grph
    val D=Examples.Grph
    val F=Examples.ReverseGraph
    IsNaturalIsomorphism(F.rightUnit) should equal(True)
  }
  
  "rightCounit" should "be an isomorphism for translations that are isomorphisms" in {
    val C=Examples.Grph
    val D=Examples.Grph
    val F=Examples.ReverseGraph
    IsNaturalIsomorphism(F.rightCounit) should equal(True)
  }
  
  "leftUnit" should "be an isomorphism for translations that are equivalences" in {
    val C=Examples.IndiscreteCategory(3)
    val D=Examples.TerminalCategory
    val F=Examples.TerminalFunctor
    IsNaturalIsomorphism(F.leftUnit) should equal(True)
  }
  
   "leftCounit" should "be an isomorphism for translations that are equivalences" in {
    val C=Examples.IndiscreteCategory(3)
    val D=Examples.TerminalCategory
    val F=Examples.TerminalFunctor
    IsNaturalIsomorphism(F.leftCounit) should equal(True)
  }
   
    "rightUnit" should "be an isomorphism for translations that are equivalences" in {
    val C=Examples.IndiscreteCategory(3)
    val D=Examples.TerminalCategory
    val F=Examples.TerminalFunctor
    IsNaturalIsomorphism(F.rightUnit) should equal(True)
  }
    
     "rightCounit" should "be an isomorphism for translations that are equivalences" in {
    val C=Examples.IndiscreteCategory(3)
    val D=Examples.TerminalCategory
    val F=Examples.TerminalFunctor
    IsNaturalIsomorphism(F.rightCounit) should equal(True)
  }
  
  "leftUnit" should "be an isomorphism for fully faithful transformations" in {
    val C=Examples.Chain(3)
    val D=Examples.Chain(4)
    val F=Examples.Skip(3,2)
    IsNaturalIsomorphism(F.leftUnit) should equal(True)
  } 
  
  "leftcoUnit" should "be an isomorphism for fully faithful transformations" in {
    val C=Examples.Chain(3)
    val D=Examples.Chain(4)
    val F=Examples.Skip(3,2)
    IsNaturalIsomorphism(F.leftCounit) should equal(True)
  } 
  
  
  }
  
}