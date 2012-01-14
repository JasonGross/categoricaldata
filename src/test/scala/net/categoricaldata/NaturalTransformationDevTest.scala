package net.categoricaldata

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.api.Ontology
import net.categoricaldata.api.FiniteMorphisms
import net.categoricaldata.api.Ontologies
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
import net.categoricaldata.api.Translation
/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class NaturalTransformationDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._
  
//  def IsNaturalIsomorphism(t:NaturalTransformation) : Boolean ={???}
//
//  "leftUnit" should "be an isomorphism for translations that are isomorphisms" in {
//    val C=Examples.Grph
//    val D=Examples.Grph
//    val F=Examples.ReverseGraph
//    IsNaturalIsomorphism(F.leftUnit) should equal(True)
//  }
//  
//  "leftCounit" should "be an isomorphism for translations that are isomorphisms" in {
//    val C=Examples.Grph
//    val D=Examples.Grph
//    val F=Examples.ReverseGraph
//    IsNaturalIsomorphism(F.leftCounit) should equal(True)
//  }
//  
//  "rightUnit" should "be an isomorphism for translations that are isomorphisms" in {
//    val C=Examples.Grph
//    val D=Examples.Grph
//    val F=Examples.ReverseGraph
//    IsNaturalIsomorphism(F.rightUnit) should equal(True)
//  }
//  
//  "rightCounit" should "be an isomorphism for translations that are isomorphisms" in {
//    val C=Examples.Grph
//    val D=Examples.Grph
//    val F=Examples.ReverseGraph
//    IsNaturalIsomorphism(F.rightCounit) should equal(True)
//  }
//  
//  "leftUnit" should "be an isomorphism for translations that are equivalences" in {
//    val C=Examples.IndiscreteCategory(3)
//    val D=Examples.TerminalCategory
//    val F=Examples.TerminalFunctor(C)
//    IsNaturalIsomorphism(F.leftUnit) should equal(True)
//  }
//  
//   "leftCounit" should "be an isomorphism for translations that are equivalences" in {
//    val C=Examples.IndiscreteCategory(3)
//    val D=Examples.TerminalCategory
//    val F=Examples.TerminalFunctor(C)
//    IsNaturalIsomorphism(F.leftCounit) should equal(True)
//  }
//   
//    "rightUnit" should "be an isomorphism for translations that are equivalences" in {
//    val C=Examples.IndiscreteCategory(3)
//    val D=Examples.TerminalCategory
//    val F=Examples.TerminalFunctor(C)
//    IsNaturalIsomorphism(F.rightUnit) should equal(True)
//  }
//    
//     "rightCounit" should "be an isomorphism for translations that are equivalences" in {
//    val C=Examples.IndiscreteCategory(3)
//    val D=Examples.TerminalCategory
//    val F=Examples.TerminalFunctor(C)
//    IsNaturalIsomorphism(F.rightCounit) should equal(True)
//  }
//  
//  "leftUnit" should "be an isomorphism for fully faithful transformations" in {
//    val C=Examples.Chain(3)
//    val D=Examples.Chain(4)
//    val F=Examples.Skip(3,2)
//    IsNaturalIsomorphism(F.leftUnit) should equal(True)
//  } 
//  
//  "leftcoUnit" should "be an isomorphism for fully faithful transformations" in {
//    val C=Examples.Chain(3)
//    val D=Examples.Chain(4)
//    val F=Examples.Skip(3,2)
//    IsNaturalIsomorphism(F.leftCounit) should equal(True)
//  } 
  
  
  
}