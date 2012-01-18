package net.categoricaldata

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.ontology.Ontology
import net.categoricaldata.ontology.Ontologies
import net.categoricaldata.ontology.Translation
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class NaturalTransformationDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._
  
//  def IsNaturalIsomorphism(t:NaturalTransformation) : Boolean ={???}
//
//TODO (Scott) These are important tests. Can you make them compile? 
//  Knowing that unit and counit work correctly will validate a lot of code, I think. 
  
  
//  "leftUnit" should "be an isomorphism for translations that are isomorphisms" in
//    val C=Examples.Grph
//    val D=Examples.Grph
//    val X=GraphDataset120114
//    val F=Examples.ReverseGraph
//    IsNaturalIsomorphism(F.leftUnit(X)) should equal(True)
//  }
//  
//  "leftCounit" should "be an isomorphism for translations that are isomorphisms" in {
//    val C=Examples.Grph
//    val D=Examples.Grph
//    val X=GraphDataset120114
//    val F=Examples.ReverseGraph
//    IsNaturalIsomorphism(F.leftCounit(X)) should equal(True)
//  }
//  
//  "rightUnit" should "be an isomorphism for translations that are isomorphisms" in {
//    val C=Examples.Grph
//    val D=Examples.Grph
//    val X=GraphDataset120114
//    val F=Examples.ReverseGraph
//    IsNaturalIsomorphism(F.rightUnit(X)) should equal(True)
//  }
//  
//  "rightCounit" should "be an isomorphism for translations that are isomorphisms" in {
//    val C=Examples.Grph
//    val D=Examples.Grph
//    val X=GraphDataset120114
//    val F=Examples.ReverseGraph
//    IsNaturalIsomorphism(F.rightCounit(X)) should equal(True)
//  }
//  
//  "leftUnit" should "be an isomorphism for translations that are equivalences" in {
//    val C=Examples.IndiscreteCategory(3)
//    val D=Examples.TerminalCategory
//    val X=GraphDataset120114
//    val F=Ontologies.morphismToTerminalObject(C)
//    IsNaturalIsomorphism(F.leftUnit(X)) should equal(True)
//  }
//  
//   "leftCounit" should "be an isomorphism for translations that are equivalences" in {
//    val C=Examples.IndiscreteCategory(3)
//    val D=Examples.TerminalCategory
//    val X=Indiscrete3Dataset120113  
//    val F=Ontologies.morphismToTerminalObject(C)
//    IsNaturalIsomorphism(F.leftCounit(X)) should equal(True)
//  }
//   
//    "rightUnit" should "be an isomorphism for translations that are equivalences" in {
//    val C=Examples.IndiscreteCategory(3)
//    val D=Examples.TerminalCategory
//    val X=Indiscrete3Dataset120113
//    val F=Ontologies.morphismToTerminalObject(C)
//    IsNaturalIsomorphism(F.rightUnit(X)) should equal(True)
//  }
//    
//     "rightCounit" should "be an isomorphism for translations that are equivalences" in {
//    val C=Examples.IndiscreteCategory(3)
//    val D=Examples.TerminalCategory
//    val X=Indiscrete3Dataset120113
//    val F=Ontologies.morphismToTerminalObject(C)
//    IsNaturalIsomorphism(F.rightCounit) should equal(True)
//  }
//  
//  "leftUnit" should "be an isomorphism for fully faithful transformations" in {
//    val C=Examples.Chain(3)
//    val D=Examples.Chain(4)
// 	  val X=Chain3Dataset120114
//    val F=Examples.Skip(3,2)
//    IsNaturalIsomorphism(F.leftUnit(X)) should equal(True)
//  } 
//  
//  "leftcoUnit" should "be an isomorphism for fully faithful transformations" in {
//    val C=Examples.Chain(3)
//    val D=Examples.Chain(4)
//	  val X=Chain3Dataset120114  
//    val F=Examples.Skip(3,2)
//    IsNaturalIsomorphism(F.leftCounit(X)) should equal(True)
//  } 
 
  val GraphDataset120114 = Dataset(source = Examples.Grph,
    onObjects = Map(
      "an edge" -> List("f", "g", "h", "i", "j"),
      "a vertex" -> List("A", "B", "C", "D")),
    onMorphisms = Map(
      ("an edge" --- "has as source" --> "a vertex") -> Map(
        "f" -> "B",
        "g" -> "B",
        "h" -> "C",
        "i" -> "C",
        "j" -> "C"),
      ("an edge" --- "has as target" --> "a vertex") -> Map(
        "f" -> "A",
        "g" -> "A",
        "h" -> "B",
        "i" -> "A",
        "j" -> "C")))
  
  val Indiscrete3Dataset120113 = Dataset(
    source = Examples.Isomorphism,
    onObjects = Map(
      "0" -> List("a1", "b1", "b2", "c1", "c2", "c3"),
      "1" -> List("A1", "B1", "B2", "C1", "C2", "C3"),
      "2" -> List("u", "v", "w", "x", "y", "z")
    ),
    onMorphisms = Map(
      ("0" --- "E00" --> "0") -> Map(
        "a1" -> "a1",
        "b1" -> "b1",
        "b2" -> "b2",
        "c1" -> "c1",
        "c2" -> "c2",
        "c3" -> "c3"
      ),("0" --- "E01" --> "1") -> Map(
        "a1" -> "A1",
        "b1" -> "B1",
        "b2" -> "B2",
        "c1" -> "C1",
        "c2" -> "C2",
        "c3" -> "C3"
      ),
      ("0" --- "E02" --> "2") -> Map(
        "a1" -> "u",
        "b1" -> "v",
        "b2" -> "w",
        "c1" -> "x",
        "c2" -> "y",
        "c3" -> "z"
      ),
      ("1" --- "E10" --> "0") -> Map(
        "A1" -> "a1",
        "B1" -> "b1",
        "B2" -> "b2",
        "C1" -> "c1",
        "C2" -> "c2",
        "C3" -> "c3"
      ),
      ("1" --- "E11" --> "1") -> Map(
        "A1" -> "A1",
        "B1" -> "B1",
        "B2" -> "B2",
        "C1" -> "C1",
        "C2" -> "C2",
        "C3" -> "C3"
      ),
      ("1" --- "E12" --> "2") -> Map(
        "A1" -> "u",
        "B1" -> "v",
        "B2" -> "w",
        "C1" -> "x",
        "C2" -> "y",
        "C3" -> "z"
      ),
      ("2" --- "E20" --> "0") -> Map(
        "u" -> "a1",
        "v" -> "b1",
        "w" -> "b2",
        "x" -> "c1",
        "y" -> "c2",
        "z" -> "c3"
      ),
      ("2" --- "E21" --> "1") -> Map(
        "u" -> "A1",
        "v" -> "B1",
        "w" -> "B2",
        "x" -> "C1",
        "y" -> "C2",
        "z" -> "C3"
      ),
      ("2" --- "E10" --> "2") -> Map(
        "u" -> "u",
        "v" -> "v",
        "w" -> "w",
        "x" -> "x",
        "y" -> "y",
        "z" -> "z"
      )
    )
  )
  
  val Chain3Dataset120114 = Dataset(source=Examples.Chain(3),
  	 onObjects = Map(
      "V0" -> List(),//Deliberately left as empty list, representing empty-set.
      "V1" -> List("1a","1b","1c","1d","1e"),
      "V2" -> List ("2f","2g","2h"),
      "V3" -> List ("3i", "3j","3k","3l")
     ),
     onMorphisms = Map(
       ("V0" --- "E01" --> "V1") -> Map(
       ), //Deliberately left empty.
       ("V1" --- "E01" --> "V2") -> Map(
           "1a" -> "2f",
           "1b" -> "2g",
           "1c" -> "2g",
           "1d" -> "2h",
           "1e" -> "2h"
       ),
       ("V2" --- "E01" --> "V3") -> Map(
           "2f" -> "3i",
           "2g" -> "3k",
           "2h" -> "3l"
       )
    )
 )
          
}