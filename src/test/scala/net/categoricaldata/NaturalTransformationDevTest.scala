package net.categoricaldata

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.categoricaldata.ontology._
import net.categoricaldata.examples.Examples
import net.categoricaldata.util.CustomMatchers
/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class NaturalTransformationDevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

  //TODO (Scott) These are important tests. Can you make them compile? 
  //  Knowing that unit and counit work correctly will validate a lot of code, I think. 

  "leftUnit" should "be an isomorphism for translations that are isomorphisms" in {
    val F = Examples.ReverseGraph
    // Apologies for this circumlocution. The compiler can't deduce by itself that F.target and GraphDataset120114 are both 'Graph'.
    val X = F.target.internalize(GraphDataset120114)
    F.pullback.leftUnit(X) should be('isomorphism_?)
  }

  "leftCounit" should "be an isomorphism for translations that are isomorphisms" in {
    val F = Examples.ReverseGraph
    val X = F.source.internalize(GraphDataset120114)
    F.pullback.leftCounit(X) should be('isomorphism_?)
  }

  "rightUnit" should "be an isomorphism for translations that are isomorphisms" in {
    val F = Examples.ReverseGraph
    val X = F.source.internalize(GraphDataset120114)
    F.pullback.rightUnit(X) should be('isomorphism_?)
  }

  "rightCounit" should "be an isomorphism for translations that are isomorphisms" in {
    val F = Examples.ReverseGraph
    val X = F.target.internalize(GraphDataset120114)
    F.pullback.rightCounit(X) should be('isomorphism_?)
  }

  "leftUnit" should "be an isomorphism for translations that are equivalences" in {
    val C = Examples.IndiscreteCategory(3)
    val F = Ontologies.morphismToTerminalObject(C)
    val X = F.target.internalize(Set120121)
    F.pullback.leftUnit(X) should be('isomorphism_?)
  }

  "leftCounit" should "be an isomorphism for translations that are equivalences" in {
    val C = Examples.IndiscreteCategory(3)
    val F = Ontologies.morphismToTerminalObject(C)
    val X = F.source.internalize(Indiscrete3Dataset120113)
    F.pullback.leftCounit(X) should be('isomorphism_?)
  }

  "rightUnit" should "be an isomorphism for translations that are equivalences" in {
    val C = Examples.IndiscreteCategory(3)
    val F = Ontologies.morphismToTerminalObject(C)
    val X = F.source.internalize(Indiscrete3Dataset120113)
    F.pullback.rightUnit(X) should be('isomorphism_?)
  }

  "rightCounit" should "be an isomorphism for translations that are equivalences" in {
    val C = Examples.IndiscreteCategory(3)
    val F = Ontologies.morphismToTerminalObject(C)
    val X = F.target.internalize(Set120121)
    F.pullback.rightCounit(X) should be('isomorphism_?)
  }

  "rightUnit" should "be an isomorphism for fully faithful transformations" in {//I changed this from leftUnit to rightUnit // TODO (David) but you didn't actually change the code, below.
    val F = Examples.Skip(3, 2)
    val X = F.target.internalize(Chain3Dataset120114) //TODO (David) Change to F.source.internalize
    F.pullback.leftUnit(X) should be('isomorphism_?) // TODO (David) Change to rightUnit
  }

  "leftCounit" should "be an isomorphism for fully faithful transformations" in {
    val F = Examples.Skip(3, 2)
    val X = F.source.internalize(Chain3Dataset120114)
    F.pullback.leftCounit(X) should be('isomorphism_?)
  }

  "leftCounit" should "be an injection for functor from Graph to Chain1" in {
    val F = Examples.GraphToFunction
    val X = F.source.internalize(GraphDataset120114)
    F.pullback.leftCounit(X) should be('injection_?)
  }

  "leftUnit" should "be a surjection for functor from Graph to Chain1" in {
    val F = Examples.GraphToFunction
    val X = F.target.internalize(DavidsFunkyFunction)
    F.pullback.leftUnit(X) should be('surjection_?)
  }

  "leftUnit" should "be an isomorphism for epi-like functors (?)" in {
    //I don't have the internet, and I don't recall the name for this type of functor, but Graph-->Chain(1) would be sone.
    val F = Examples.GraphToFunction
    val X = DavidsFunkyFunction
    //TODO (Scott) this line should be replaced by F.pullback.leftUnit(X) should be('isomorphism_?), but this causes a compiler crash. Mimicking your fix with internalize doesn't seem to help.
    //TODO (David) It seems to work fine for me. Can you show me the code that doesn't compile?
    // aside --- a compiler crash is very different than the compiler telling you the code is invalid. The compiler *does* crash sometimes, so it's worth knowing the difference.
    F.pullback.leftUnit should be('isomorphism_?)
  }

  "rightCounit" should "be an isomorphism for epi-like functors (?)" in {
    //I don't have the internet, and I don't recall the name for this type of functor, but Graph-->Chain(1) would be sone.
    val F = Examples.GraphToFunction
    val X = DavidsFunkyFunction
    //TODO (Scott) this line should be replaced by F.pullback.rightCounit(X) should be('isomorphism_?), but this causes a compiler crash. Mimicking your fix with internalize doesn't seem to help.
    //TODO (David) Seems to work fine for me, again, can you show me the code?
    F.pullback.rightCounit should be('isomorphism_?) 
  }

  val GraphDataset120114 = Dataset(source = Examples.Graph,
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
    source = Examples.IndiscreteCategory(3),
    onObjects = Map(
      "V1" -> List("a1", "b1", "b2", "c1", "c2", "c3"),
      "V2" -> List("A1", "B1", "B2", "C1", "C2", "C3"),
      "V3" -> List("u", "v", "w", "x", "y", "z")),
    onMorphisms = Map(
      ("V1" --- "E11" --> "V1") -> Map(
        "a1" -> "a1",
        "b1" -> "b1",
        "b2" -> "b2",
        "c1" -> "c1",
        "c2" -> "c2",
        "c3" -> "c3"), 
      ("V1" --- "E12" --> "V2") -> Map(
        "a1" -> "A1",
        "b1" -> "B1",
        "b2" -> "B2",
        "c1" -> "C1",
        "c2" -> "C2",
        "c3" -> "C3"),
      ("V1" --- "E13" --> "V3") -> Map(
        "a1" -> "u",
        "b1" -> "v",
        "b2" -> "w",
        "c1" -> "x",
        "c2" -> "y",
        "c3" -> "z"),
      ("V2" --- "E21" --> "V1") -> Map(
        "A1" -> "a1",
        "B1" -> "b1",
        "B2" -> "b2",
        "C1" -> "c1",
        "C2" -> "c2",
        "C3" -> "c3"),
      ("V2" --- "E22" --> "V2") -> Map(
        "A1" -> "A1",
        "B1" -> "B1",
        "B2" -> "B2",
        "C1" -> "C1",
        "C2" -> "C2",
        "C3" -> "C3"),
      ("V2" --- "E23" --> "V3") -> Map(
        "A1" -> "u",
        "B1" -> "v",
        "B2" -> "w",
        "C1" -> "x",
        "C2" -> "y",
        "C3" -> "z"),
      ("V3" --- "E31" --> "V1") -> Map(
        "u" -> "a1",
        "v" -> "b1",
        "w" -> "b2",
        "x" -> "c1",
        "y" -> "c2",
        "z" -> "c3"),
      ("V3" --- "E32" --> "V2") -> Map(
        "u" -> "A1",
        "v" -> "B1",
        "w" -> "B2",
        "x" -> "C1",
        "y" -> "C2",
        "z" -> "C3"),
      ("V3" --- "E33" --> "V3") -> Map(
        "u" -> "u",
        "v" -> "v",
        "w" -> "w",
        "x" -> "x",
        "y" -> "y",
        "z" -> "z")))

  val Chain3Dataset120114 = Dataset(source = Examples.Chain(3),
    onObjects = Map(
      "V0" -> List(), //Deliberately left as empty list, representing empty-set.
      "V1" -> List("1a", "1b", "1c", "1d", "1e"),
      "V2" -> List("2f", "2g", "2h"),
      "V3" -> List("3i", "3j", "3k", "3l")),
    onMorphisms = Map(
      ("V0" --- "E01" --> "V1") -> Map[String, String](), //Deliberately left empty.
      ("V1" --- "E12" --> "V2") -> Map(
        "1a" -> "2f",
        "1b" -> "2g",
        "1c" -> "2g",
        "1d" -> "2h",
        "1e" -> "2h"),
      ("V2" --- "E23" --> "V3") -> Map(
        "2f" -> "3i",
        "2g" -> "3k",
        "2h" -> "3l")))

  val DavidsFunkyFunction = Dataset(source = Examples.Chain(1),
    onObjects = Map(
      "V0" -> List("David", "Scott", "UC Berkeley", "MIT"),
      "V1" -> List("1978", "Scott's birthyear", "1868", "1861")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "David" -> "1978",
        "Scott" -> "Scott's birthyear",
        "UC Berkeley" -> "1868",
        "MIT" -> "1861")))
  
   val Set120121 = Dataset(source = Examples.Chain(0),
       onObjects = Map ("V0" -> List("a","b","c","d")),
       onMorphisms = Map()
   )
}