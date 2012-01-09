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
/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class DevTest extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._

  val DavidsFunkyGraph = Dataset(source = Examples.Grph,
    onObjects = Map(
      "an edge" -> List("f", "g", "h", "i", "j"),
      "a vertex" -> List("A", "B", "C", "D")),
    onMorphisms = Map(
      ("an edge" --- "has as source" --> "a vertex") -> Map(
        "f" -> "A",
        "g" -> "A",
        "h" -> "B",
        "i" -> "A",
        "j" -> "C"),
      ("an edge" --- "has as target" --> "a vertex") -> Map(
        "f" -> "B",
        "g" -> "B",
        "h" -> "C",
        "i" -> "C",
        "j" -> "C")))

  val DavidsFunkyDiscreteDynamicalSystem = Dataset(source = Examples.DiscreteDynamicalSystem,
    onObjects = Map(
      "an element" -> List("fhjbar", "ghjbar", "ijbar", "hjbar", "jbar")),
    onMorphisms = Map(
      ("an element" --- "has as successor" --> "an element") -> Map(
        "fhjbar" -> "hjbar",
        "ghjbar" -> "hjbar",
        "ijbar" -> "jbar",
        "hjbar" -> "jbar",
        "jbar" -> "jbar")))

  val GraphFromDavidsFunkyDiscreteDynamicalSystem = Dataset(source = Examples.Grph,
    onObjects = Map(
      "a vertex" -> List("fhjbar", "ghjbar", "ijbar", "hjbar", "jbar"),
      "an edge" -> List("f", "g", "h", "i", "j")),
    onMorphisms = Map(
      ("an edge" --- "has as source" --> "a vertex") -> Map(
        "f" -> "fhjbar",
        "g" -> "ghjbar",
        "h" -> "hjbar",
        "i" -> "ijbar",
        "j" -> "jbar"),
      ("an edge" --- "has as target" --> "a vertex") -> Map(
        "f" -> "hjbar",
        "g" -> "hjbar",
        "h" -> "jbar",
        "i" -> "jbar",
        "j" -> "jbar")))

  val DavidsFunkyGraphReversed = Dataset(source = Examples.Grph,
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

  val FunkyE2Dataset = Dataset(
    source = Examples.E2,
    onObjects = Map(
      "0" -> List("a", "b", "c", "d"),
      "1" -> List("1", "2", "3")),
    onMorphisms = Map(
      "0" --- "E01" --> "1" -> Map(
        "a" -> "1",
        "b" -> "1",
        "c" -> "2",
        "d" -> "3"),
      "1" --- "E10" --> "0" -> Map(
        "1" -> "a",
        "2" -> "b",
        "3" -> "d")))

  val E2ToPointedSetsRPushFunky = Dataset(
    source = Examples.PointedSets,
    onObjects = Map(
      "an element" -> List("a1a", "b1a", "d3d"),
      "a pointed set" -> List("a1", "d3")),
    onMorphisms = Map(
      ("an element" --- "is in" --> "a pointed set") -> Map(
        "a1a" -> "a1",
        "ba1" -> "a1",
        "d3d" -> "d3"),
      ("a pointed set" --- "has as chosen" --> "an element") -> Map(
        "a1" -> "a1a",
        "d3" -> "d3d")))

  val OneTwoThreePointed = Dataset(
    source = Examples.PointedSets,
    onObjects = Map(
      "an element" -> List("a1", "b1", "b2", "c1", "c2", "c3"),
      "a pointed set" -> List("a", "b", "c")),
    onMorphisms = Map(
      ("an element" --- "is in" --> "a pointed set") -> Map(
        "a1" -> "a",
        "b1" -> "b",
        "b2" -> "b",
        "c1" -> "c",
        "c2" -> "c",
        "c3" -> "c"),
      ("a pointed set" --- "has as chosen" --> "an element") -> Map(
        "a" -> "a1",
        "b" -> "b1",
        "c" -> "c1")))

  val OneTwoThree = Dataset(
    source = Examples.Chain(1),
    onObjects = Map(
      "an element" -> List("a1", "b1", "b2", "c1", "c2", "c3"),
      "a pointed set" -> List("a", "b", "c")),
    onMorphisms = Map(
      ("an element" --- "is in" --> "a pointed set") -> Map(
        "a1" -> "a",
        "b1" -> "b",
        "b2" -> "b",
        "c1" -> "c",
        "c2" -> "c",
        "c3" -> "c"),
      ("a pointed set" --- "has as chosen" --> "an element") -> Map(
        "a" -> "a1",
        "b" -> "b1",
        "c" -> "c1")))      
        
  val SixElementsIso = Dataset(
    source = Examples.Isomorphism,
    onObjects = Map(
      "0" -> List("a1", "b1", "b2", "c1", "c2", "c3"),
      "1" -> List("a1", "b1", "b2", "c1", "c2", "c3")),
    onMorphisms = Map(
      ("0" --- "E01" --> "1") -> Map(
        "a1" -> "a1",
        "b1" -> "b1",
        "b2" -> "b2",
        "c1" -> "c1",
        "c2" -> "c2",
        "c3" -> "c3"),
      ("1" --- "E10" --> "0") -> Map(
        "a1" -> "a1",
        "b1" -> "b1",
        "b2" -> "b2",
        "c1" -> "c1",
        "c2" -> "c2",
        "c3" -> "c3")))

  val ThreeElementsIso = Dataset(
    source = Examples.Isomorphism,
    onObjects = Map(
      "0" -> List("a", "b", "c"),
      "1" -> List("a", "b", "c")),
    onMorphisms = Map(
      ("0" --- "E01" --> "1") -> Map(
        "a" -> "a",
        "b" -> "b",
        "c" -> "c"),
      ("1" --- "E10" --> "0") -> Map(
        "a" -> "a",
        "b" -> "b",
        "c" -> "c")))

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

  val DavidsFunkyFiniteCyclicMonoid = Dataset(
    source = Examples.FiniteCyclicMonoid(2, 1),
    onObjects = Map("an element" -> List("David", "Scott", "UC Berkeley", "MIT", "succDavid", "succScott", "succUC Berkeley", "succMIT")),
    onMorphisms = Map("an element" --- "has as successor" --> "an element" -> Map(
      "David" -> "succDavid",
      "Scott" -> "succScott",
      "UC Berkeley" -> "succUC Berkeley",
      "MIT" -> "succMIT",
      "succDavid" -> "succDavid",
      "succScott" -> "succScott",
      "succUC Berkeley" -> "succUC Berkeley",
      "succMIT" -> "succMIT")))

  val DavidsFunkySet1 = Dataset(source = Examples.Chain(0),
    onObjects = Map(
      "V0" -> List("David", "Scott", "UC Berkeley", "MIT")),
    onMorphisms = Map())

  val DavidsFunkySet2 = Dataset(source = Examples.Chain(0),
    onObjects = Map(
      "V0" -> List("1978", "Scott's birthyear", "1868", "1861")),
    onMorphisms = Map())

  "__*" should "work nicely with the map from E2 to PointedSets" in {
    Examples.E2ToPointedSets.__*(FunkyE2Dataset) should beIsomorphicTo(E2ToPointedSetsRPushFunky)
  }

  //   "__!" should "work (1)" in {
  //	   TerminalCategoryToFiniteCyclicMonoid(2,1).__!(DavidsFunkySet1) should equal (DavidsFunkyFiniteCyclicMonoid)
  //   }
  //
  //  "__*" should "work (2)" in {
  //    GraphToDiscreteDynamicalSystem1.__*(DavidsFunkyGraph).isIsomorphicTo(DavidsFunkyDiscreteDynamicalSystem) should equal(true)
  //  }

  //   
  //   "__*" should "preserve the terminal dataset" in {
  //	   TerminalCategoryToFiniteCyclicMonoid(10,7).__*(TerminalDataset(TerminalCategory)) 
  //	   should equal 
  //	   (TerminalDataset(FiniteCyclicMonoid(10,7)))
  //   }
  //   
  //   "__!" should "preserve the initialdataset" in {
  //	   TerminalCategoryToFiniteCyclicMonoid(10,7).__!(InitialDataset(TerminalCategory)) 
  //	   should equal 
  //	   (InitialDataset(FiniteCyclicMonoid(10,7)))
  //   }

  

  "__*" should "reverse graph as expected" in {
    println
    println("Output from \"__* should reverse graph as expected\":")

    val X = DavidsFunkyGraph
    val LHS = DavidsFunkyGraphReversed
    val RHS = Examples.ReverseGraph.__*(DavidsFunkyGraph)
    println("Original graph: " + X)
    println("Reversed graph: " + LHS)
    println("Right pushforward of original: " + RHS)
    LHS should beIsomorphicTo(RHS)
  }

  
  // TODO (Scott): Can the following two tests be made "generic" in the way I want them to? See comments.
  // Scott: sure, if you have some supply of datasets, you could write
  /*
   * for(dataset <- dataset; C = dataset.source) {
   *    val T = TerminalFunctor(C)
   *    val x = T.target.allObjects.head 									// the target only has one object, so this hack to get it is okay.
   * 	dataset.colimitSet should equal(TerminalFunctor(C).__!(dataset)(x))
   * }
   */
  

  "__* along PointedSetsToIsomorphism" should "take a retraction and return two sets isomorphic to its base." in {
    println
    println("Output from \"__* along PointedSetsToIsomorphism should take a retraction and return two sets isomorphic to its base.\":")
    println
    val X = OneTwoThreePointed
    val LHS = SixElementsIso
    val RHS = Examples.PointedSetsToIsomorphism.__*(X)
    println("Original retraction: "); println(X); println
    println("Expected isomorphism: "); println(LHS); println
    println("Right pushforward of original retraction: "); println(RHS)
    LHS should beIsomorphicTo(RHS)
  }

  
  
  val SaturatedCommutativeTriangle = { 
    val ArrowsList=List (
          "V0" --- "E00" --> "V0",
          "V0" --- "E01" --> "V1",
          "V0" --- "E02" --> "V2",
          "V1" --- "E11" --> "V1",
          "V1" --- "E12" --> "V2",
          "V2" --- "E22" --> "V2")
    val RelationsId0=List(
        ("V0" --- "E00" --> "V0" --- "E00" --> "V0") === ("V0" --- "E00" --> "V0"),
        ("V0" --- "E00" --> "V0" --- "E01" --> "V1") === ("V0" --- "E01" --> "V1"),
        ("V0" --- "E00" --> "V0" --- "E02" --> "V2") === ("V0" --- "E02" --> "V2"))
    val RelationsId1=List(
        ("V0" --- "E01" --> "V1" --- "E11" --> "V1") === ("V0" --- "E01" --> "V1"),
        ("V1" --- "E11" --> "V1" --- "E11" --> "V1") === ("V1" --- "E11" --> "V1"),
        ("V1" --- "E11" --> "V1" --- "E12" --> "V2") === ("V1" --- "E12" --> "V2"))
    val RelationsId2=List(
        ("V0" --- "E02" --> "V2" --- "E22" --> "V2") === ("V0" --- "E02" --> "V2"),
        ("V1" --- "E12" --> "V2" --- "E22" --> "V2") === ("V1" --- "E12" --> "V2"),
        ("V2" --- "E22" --> "V2" --- "E22" --> "V2") === ("V2" --- "E22" --> "V2"))
    val Comp012=List(
        ("V0" --- "E01" --> "V1" --- "E12" --> "V2") === ("V0" --- "E02" --> "V2"))
    
    Ontology(
  
      objects = List("V0","V1","V2"),
      arrows = ArrowsList,
      relations = List.concat(RelationsId0,RelationsId1,RelationsId2,Comp012)
    )
  }
//  
//  "BeIsomorphicAsCategoriesTo" should "see the iso between saturated commutative triangle and Chain2" in { //TODO Is this test in the right place? Does it work? Do we have BeIsomorphicAsCategoriesTo, or any other such guy?
//    chain(2) should BeIsomorphicAsCategoriesTo(SaturatedCommutativeTriangle)
//  }
      
  val DDS1 = Dataset(source = Examples.DiscreteDynamicalSystem,
    onObjects = Map(
      "an element" -> List("a", "b", "c", "d")),
    onMorphisms = Map(
      "an element" --- "has as successor" --> "an element" -> Map(
        "a" -> "c",
        "b" -> "c",
        "c" -> "d",
        "d" -> "d")))

  val DDS1Times2L = Dataset(source = Examples.DiscreteDynamicalSystem,
    onObjects = Map(
      "an element" -> List("a1", "b1", "c1", "d1", "a2", "b2", "c2", "d2")),
    onMorphisms = Map(
      "an element" --- "has as successor" --> "an element" -> Map(
        "a1" -> "a2",
        "b1" -> "b2",
        "c1" -> "c2",
        "d1" -> "d2",
        "a2" -> "c1",
        "b2" -> "c1",
        "c2" -> "d1",
        "d2" -> "d1")))

  val DDS1Times2R = Dataset(source = Examples.DiscreteDynamicalSystem,
    onObjects = Map(
      "an element" -> List("aa", "ab", "ac", "ad", "ba", "bb", "bc", "bd", "ca", "cb", "cc", "cd", "da", "db", "dc", "dd")),
    onMorphisms = Map(
      "an element" --- "has as successor" --> "an element" -> Map(
        "aa" -> "ca",
        "ab" -> "ca",
        "ac" -> "da",
        "ad" -> "da",
        "ba" -> "cb",
        "bb" -> "cb",
        "bc" -> "db",
        "bd" -> "db",
        "ca" -> "cc",
        "cb" -> "cc",
        "cc" -> "dc",
        "cd" -> "dc",
        "da" -> "cd",
        "db" -> "cd",
        "dc" -> "dd",
        "dd" -> "dd")))

//      val DDSTimeLapse2 = Translation (
//        source = Examples.DiscreteDynamicalSystem,
//        target = Examples.DiscreteDynamicalSystem,
//        onObjects = Map ("an element" -> "an element"),
//        onMorphisms = Map (
//            ("an element" --- "has as successor" --> "an element") -> 
//            ("an element" --- "has as successor" --> "an element" --- "has as successor" --> "an element")))
//          	
//
//     "__*" should "provide a 'half-speed' DDS" in {
//      println
//      println("Output from \"__* should provide a 'half-speed' DDS\":")
//      
//      val X = DDS1
//      val LHS = DDSTimeLapse2.__*(X)
//      val RHS = DDS1Times2R
//      println(X)
//      println(LHS)
//      println(RHS)
//      LHS should beIsomorphicTo(RHS)
//   }      
//    
//    "__!" should "provide a 'half-speed' DDS" in {
//      println
//      println("Output from \"__! should provide a 'half-speed' DDS\":")
//      
//      val X = DDS1
//      val LHS = DDSTimeLapse2.__!(X)
//      val RHS = DDS1Times2L
//      println(X)
//      println(LHS)
//      println(RHS)
//      LHS should beIsomorphicTo(RHS)
//   }      

       val FCM20_19 = Dataset(source = Examples.FiniteCyclicMonoid(20,19),
      onObjects = Map(
        "an element" -> List("a","b","c","d")),      
      onMorphisms = Map(
        "an element" --- "has as successor" --> "an element" -> Map(
          "a" -> "c",
          "b" -> "c",
          "c" -> "d",
          "d" -> "d")))
    
    val FCM20_19Times2L = Dataset(source = Examples.FiniteCyclicMonoid(20,19),
      onObjects = Map(
        "an element" -> List("a1","b1","c1","d1","a2","b2","c2","d2")),      
      onMorphisms = Map(
        "an element" --- "has as successor" --> "an element" -> Map(
          "a1" -> "a2",
          "b1" -> "b2",
          "c1" -> "c2",
          "d1" -> "d2",
          "a2" -> "c1",
          "b2" -> "c1",
          "c2" -> "d1",
          "d2" -> "d1")))
          
    val FCM20_19Times2R = Dataset(source = Examples.FiniteCyclicMonoid(20,19),
      onObjects = Map(
        "an element" -> List("aa","ab","ac","ad","ba","bb","bc","bd","ca","cb","cc","cd","da","db","dc","dd")),
      onMorphisms = Map(
        "an element" --- "has as successor" --> "an element" -> Map(
          "aa" -> "ca",
          "ab" -> "ca",
          "ac" -> "da",
          "ad" -> "da",
          "ba" -> "cb",
          "bb" -> "cb",
          "bc" -> "db",
          "bd" -> "db",
          "ca" -> "cc",
          "cb" -> "cc",
          "cc" -> "dc",
          "cd" -> "dc",
          "da" -> "cd",
          "db" -> "cd",
          "dc" -> "dd",
          "dd" -> "dd")))

// These tests run forever. What's going on?
//     "__*" should "provide a 'half-speed' FCM20_19" in {
//      println
//      println("Output from \"__* should provide a 'half-speed' FCM20_19\":")
//      
//      val X = FCM20_19
//      val T = Examples.TranslationFiniteCyclicMonoids(20,19,20,19,2);
//      val LHS = T.__*(X)
//      val RHS = FCM20_19Times2R
//      println(X)
//      println(LHS)
//      println(RHS)
//      LHS should beIsomorphicTo(RHS)
//   }      
          
            val FCM7_6 = Dataset(source = Examples.FiniteCyclicMonoid(7,6),
    onObjects = Map(
      "an element" -> List("a", "b", "c", "d")),
    onMorphisms = Map(
      "an element" --- "has as successor" --> "an element" -> Map(
        "a" -> "c",
        "b" -> "c",
        "c" -> "d",
        "d" -> "d")))

  val FCM7_6Times2L = Dataset(source = Examples.FiniteCyclicMonoid(7,6),
    onObjects = Map(
      "an element" -> List("a1", "b1", "c1", "d1", "a2", "b2", "c2", "d2")),
    onMorphisms = Map(
      "an element" --- "has as successor" --> "an element" -> Map(
        "a1" -> "a2",
        "b1" -> "b2",
        "c1" -> "c2",
        "d1" -> "d2",
        "a2" -> "c1",
        "b2" -> "c1",
        "c2" -> "d1",
        "d2" -> "d1")))

  val FCM7_6Times2R = Dataset(source = Examples.FiniteCyclicMonoid(7,6),
    onObjects = Map(
      "an element" -> List("aa", "ab", "ac", "ad", "ba", "bb", "bc", "bd", "ca", "cb", "cc", "cd", "da", "db", "dc", "dd")),
    onMorphisms = Map(
      "an element" --- "has as successor" --> "an element" -> Map(
        "aa" -> "ca",
        "ab" -> "ca",
        "ac" -> "da",
        "ad" -> "da",
        "ba" -> "cb",
        "bb" -> "cb",
        "bc" -> "db",
        "bd" -> "db",
        "ca" -> "cc",
        "cb" -> "cc",
        "cc" -> "dc",
        "cd" -> "dc",
        "da" -> "cd",
        "db" -> "cd",
        "dc" -> "dd",
        "dd" -> "dd")))

  
  // These tests run forever. What's going on?
  //     "__*" should "provide a 'half-speed' FCM20_19" in {
  //      println
  //      println("Output from \"__* should provide a 'half-speed' FCM20_19\":")
  //      
  //      val X = FCM20_19
  //      val T = Examples.TranslationFiniteCyclicMonoids(20,19,20,19,2);
  //      val LHS = T.__*(X)
  //      val RHS = FCM20_19Times2R
  //      println(X)
  //      println(LHS)
  //      println(RHS)
  //      LHS should beIsomorphicTo(RHS)
  //   }  
        
        
       "__*" should "provide a 'half-speed' FCM7_6" in {
        println
        println("Output from \"__* should provide a 'half-speed' FCM7_6\":")
        
        val X = FCM7_6
        val T = Examples.TranslationFiniteCyclicMonoids(7,6,7,6,2);
        val LHS = T.__*(X)
        val RHS = FCM7_6Times2R
        println(X)
        println(LHS)
        println(RHS)
        LHS should beIsomorphicTo(RHS)
     }      
//    
//    "__!" should "provide a 'half-speed' FCM20_19" in {
//      println
//      println("Output from \"__! should provide a 'half-speed' FCM20_19\":")
//      
//      val X = FCM20_19
//      val T = Examples.TranslationFiniteCyclicMonoids(20,19,20,19,2);
//      val LHS = T.__!(X)
//      val RHS = FCM20_19Times2L
//      println(X)
//      println(LHS)
//      println(RHS)
//      LHS should beIsomorphicTo(RHS)
//   }       
  
  "dataset" should "throw some exception if it does not conform to relations in the source" in {
    lazy val badData = Dataset (source = Examples.Isomorphism,
        onObjects = Map (
            "0" -> List ("x","y"),
            "1" -> List ("z")),
        onMorphisms = Map (
            "0" --- "E01" --> "1" -> Map (
                "x" -> "z",
                "y" -> "z"),
            "1" --- "E10" --> "0" -> Map (
                "z" -> "x")
        )
    )
    evaluating { badData } should produce [IllegalArgumentException]
  }
   
}