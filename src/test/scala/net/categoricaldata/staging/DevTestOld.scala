//package net.categoricaldata
//
//import org.scalatest.FlatSpec
//import org.scalatest.matchers.ShouldMatchers
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
//import scala.math._
//import net.categoricaldata.api.Ontology
//import net.categoricaldata.api.FiniteMorphisms
//import net.categoricaldata.api.Ontologies
//import net.categoricaldata.examples.Examples
//import net.categoricaldata.util.CustomMatchers
//import net.categoricaldata.api.Box
///*
// * This should always compile when checked in.
// */
//
//@RunWith(classOf[JUnitRunner])
//class DevTestOld extends FlatSpec with ShouldMatchers with CustomMatchers {
//  // NOTE to use the DSL, you need this line:
//  import net.categoricaldata.dsl.Sentences._
//
//  val DavidsFunkyDiscreteDynamicalSystem = Dataset(source = Examples.DiscreteDynamicalSystem,
//    onObjects = Map(
//      "an element" -> List("fhjbar", "ghjbar", "ijbar", "hjbar", "jbar")),
//    onMorphisms = Map(
//      ("an element" --- "has as successor" --> "an element") -> Map(
//        "fhjbar" -> "hjbar",
//        "ghjbar" -> "hjbar",
//        "ijbar" -> "jbar",
//        "hjbar" -> "jbar",
//        "jbar" -> "jbar")))
//
//  val GraphFromDavidsFunkyDiscreteDynamicalSystem = Dataset(source = Examples.Graph,
//    onObjects = Map(
//      "a vertex" -> List("fhjbar", "ghjbar", "ijbar", "hjbar", "jbar"),
//      "an edge" -> List("f", "g", "h", "i", "j")),
//    onMorphisms = Map(
//      ("an edge" --- "has as source" --> "a vertex") -> Map(
//        "f" -> "fhjbar",
//        "g" -> "ghjbar",
//        "h" -> "hjbar",
//        "i" -> "ijbar",
//        "j" -> "jbar"),
//      ("an edge" --- "has as target" --> "a vertex") -> Map(
//        "f" -> "hjbar",
//        "g" -> "hjbar",
//        "h" -> "jbar",
//        "i" -> "jbar",
//        "j" -> "jbar")))
//
//  val ThreeElementsIso = Dataset(
//    source = Examples.Isomorphism,
//    onObjects = Map(
//      "0" -> List("a", "b", "c"),
//      "1" -> List("a", "b", "c")),
//    onMorphisms = Map(
//      ("0" --- "E01" --> "1") -> Map(
//        "a" -> "a",
//        "b" -> "b",
//        "c" -> "c"),
//      ("1" --- "E10" --> "0") -> Map(
//        "a" -> "a",
//        "b" -> "b",
//        "c" -> "c")))
//
//  val DavidsFunkyGraphReversed = Dataset(source = Examples.Graph,
//    onObjects = Map(
//      "an edge" -> List("f", "g", "h", "i", "j"),
//      "a vertex" -> List("A", "B", "C", "D")),
//    onMorphisms = Map(
//      ("an edge" --- "has as source" --> "a vertex") -> Map(
//        "f" -> "B",
//        "g" -> "B",
//        "h" -> "C",
//        "i" -> "C",
//        "j" -> "C"),
//      ("an edge" --- "has as target" --> "a vertex") -> Map(
//        "f" -> "A",
//        "g" -> "A",
//        "h" -> "B",
//        "i" -> "A",
//        "j" -> "C")))
//
//  val FunkyE2Dataset = Dataset(
//    source = Examples.E2,
//    onObjects = Map(
//      "0" -> List("a", "b", "c", "d"),
//      "1" -> List("1", "2", "3")),
//    onMorphisms = Map(
//      "0" --- "E01" --> "1" -> Map(
//        "a" -> "1",
//        "b" -> "1",
//        "c" -> "2",
//        "d" -> "3"),
//      "1" --- "E10" --> "0" -> Map(
//        "1" -> "a",
//        "2" -> "b",
//        "3" -> "d")))
//
//  val E2ToPointedSetsRPushFunky = Dataset(
//    source = Examples.PointedSets,
//    onObjects = Map(
//      "an element" -> List("a1a", "b1a", "d3d"),
//      "a pointed set" -> List("a1", "d3")),
//    onMorphisms = Map(
//      ("an element" --- "is in" --> "a pointed set") -> Map(
//        "a1a" -> "a1",
//        "ba1" -> "a1",
//        "d3d" -> "d3"),
//      ("a pointed set" --- "has as chosen" --> "an element") -> Map(
//        "a1" -> "a1a",
//        "d3" -> "d3d")))
//
//  val OneTwoThreePointed = Dataset(
//    source = Examples.PointedSets,
//    onObjects = Map(
//      "an element" -> List("a1", "b1", "b2", "c1", "c2", "c3"),
//      "a pointed set" -> List("a", "b", "c")),
//    onMorphisms = Map(
//      ("an element" --- "is in" --> "a pointed set") -> Map(
//        "a1" -> "a",
//        "b1" -> "b",
//        "b2" -> "b",
//        "c1" -> "c",
//        "c2" -> "c",
//        "c3" -> "c"),
//      ("a pointed set" --- "has as chosen" --> "an element") -> Map(
//        "a" -> "a1",
//        "b" -> "b1",
//        "c" -> "c1")))
//
//
//  val DavidsFunkyFunction = Dataset(source = Examples.Chain(1),
//    onObjects = Map(
//      "V0" -> List("David", "Scott", "UC Berkeley", "MIT"),
//      "V1" -> List("1978", "Scott's birthyear", "1868", "1861")),
//    onMorphisms = Map(
//      "V0" --- "E01" --> "V1" -> Map(
//        "David" -> "1978",
//        "Scott" -> "Scott's birthyear",
//        "UC Berkeley" -> "1868",
//        "MIT" -> "1861")))
//
//  
//  // TODO (David) Graph to FCM2_1.
//        
//  val GraphToFCM2_1 = Translation (
//      source = Examples.Graph,
//      target = Examples.FiniteCyclicMonoid(2,1),
//      onObjects = Map (
//          "an edge" -> "an element", 
//          "a vertex" -> "an element"),
//      onMorphisms = Map (
//          ("an edge" --- "has as source" --> "a vertex") -> ("an element" --- "has as successor" --> "an element"),
//          ("an edge" --- "has as target" --> "a vertex") -> ("an element".identity)
//      )
//  )
//  
//  val DavidsGraph = Dataset (source = Examples.Graph,
//      onObjects = Map(
//          "an edge" -> List ("0","1","2","3"),
//          "a vertex" -> List ("a", "b", "c", "d")),
//      onMorphisms = Map (
//          ("an edge" --- "has as source" --> "a vertex") -> Map (
//              "a" -> "0",
//              "b" -> "0",
//              "c" -> "0",
//              "d" -> "2"),
//          ("an edge" --- "has as target" --> "a vertex") -> Map (
//              "a" -> "1",
//              "b" -> "2",
//              "c" -> "2",
//              "d" -> "2")
//      )
// )
//  
//// val DavidsGraphLFCM2_1 = Dataset(source = Examples.FiniteCyclicMonoid(2,1)) //TODO (David) Define these two FCM2_1 datasets to make the following two tests work.
//// val DavidsGraphRFCM2_1 = Dataset(source = Examples.FiniteCyclicMonoid(2,1))
//// 
//// "__!" should "when applied to DavidsGraph, return the correct FCM2_1" in {
////    val LHS = GraphToFCM2_1__!(DavidsGraph)
////    val RHS = DavidsGraphLFCM2_1
////    LHS should beIsomorphicTo(RHS)
////  }
////  
//// "__*" should "when applied to DavidsGraph, return the correct FCM2_1" in {
////    val LHS = GraphToFCM2_1__*(DavidsGraph)
////    val RHS = DavidsGraphRFCM2_1
////    LHS should beIsomorphicTo(RHS)
////  }
//      
//          
//  // TODO this doesn't work because the target is infinite!
//  "__*" should "convert a graph into a discrete dynamical system" in {
//    val DavidsFunkyGraph = Dataset(source = Examples.Graph,
//      onObjects = Map(
//        "an edge" -> List("f", "g", "h", "i", "j"),
//        "a vertex" -> List("A", "B", "C", "D")),
//      onMorphisms = Map(
//        ("an edge" --- "has as source" --> "a vertex") -> Map(
//          "f" -> "A",
//          "g" -> "A",
//          "h" -> "B",
//          "i" -> "A",
//          "j" -> "C"),
//        ("an edge" --- "has as target" --> "a vertex") -> Map(
//          "f" -> "B",
//          "g" -> "B",
//          "h" -> "C",
//          "i" -> "C",
//          "j" -> "C")))
//
//    Examples.GraphToDiscreteDynamicalSystem1.__*(DavidsFunkyGraph) should beIsomorphicTo(DavidsFunkyDiscreteDynamicalSystem)
//  }
//
//  val DDS1 = Dataset(source = Examples.DiscreteDynamicalSystem,
//    onObjects = Map(
//      "an element" -> List("a", "b", "c", "d")),
//    onMorphisms = Map(
//      "an element" --- "has as successor" --> "an element" -> Map(
//        "a" -> "c",
//        "b" -> "c",
//        "c" -> "d",
//        "d" -> "d")))
//
//  val DDS1Times2L = Dataset(source = Examples.DiscreteDynamicalSystem,
//    onObjects = Map(
//      "an element" -> List("a1", "b1", "c1", "d1", "a2", "b2", "c2", "d2")),
//    onMorphisms = Map(
//      "an element" --- "has as successor" --> "an element" -> Map(
//        "a1" -> "a2",
//        "b1" -> "b2",
//        "c1" -> "c2",
//        "d1" -> "d2",
//        "a2" -> "c1",
//        "b2" -> "c1",
//        "c2" -> "d1",
//        "d2" -> "d1")))
//
//  val DDS1Times2R = Dataset(source = Examples.DiscreteDynamicalSystem,
//    onObjects = Map(
//      "an element" -> List("aa", "ab", "ac", "ad", "ba", "bb", "bc", "bd", "ca", "cb", "cc", "cd", "da", "db", "dc", "dd")),
//    onMorphisms = Map(
//      "an element" --- "has as successor" --> "an element" -> Map(
//        "aa" -> "ca",
//        "ab" -> "ca",
//        "ac" -> "da",
//        "ad" -> "da",
//        "ba" -> "cb",
//        "bb" -> "cb",
//        "bc" -> "db",
//        "bd" -> "db",
//        "ca" -> "cc",
//        "cb" -> "cc",
//        "cc" -> "dc",
//        "cd" -> "dc",
//        "da" -> "cd",
//        "db" -> "cd",
//        "dc" -> "dd",
//        "dd" -> "dd")))
//
//  val DDSTimeLapse2 = Translation(
//    source = Examples.DiscreteDynamicalSystem,
//    target = Examples.DiscreteDynamicalSystem,
//    onObjects = Map("an element" -> "an element"),
//    onMorphisms = Map(
//      ("an element" --- "has as successor" --> "an element") ->
//        ("an element" --- "has as successor" --> "an element" --- "has as successor" --> "an element")))
//
//  // TODO this doesn't work because the target is infinite!
//  "__*" should "provide a 'half-speed' DDS" in {
//    println
//    println("Output from \"__* should provide a 'half-speed' DDS\":")
//
//    val X = DDS1
//    val LHS = DDSTimeLapse2.__*(X)
//    val RHS = DDS1Times2R
//    println(X)
//    println(LHS)
//    println(RHS)
//    LHS should beIsomorphicTo(RHS)
//  }
//
//  // TODO this doesn't work because the target is infinite!
//  "__!" should "provide a 'half-speed' DDS" in {
//    println
//    println("Output from \"__! should provide a 'half-speed' DDS\":")
//
//    val X = DDS1
//    val LHS = DDSTimeLapse2.__!(X)
//    val RHS = DDS1Times2L
//    println(X)
//    println(LHS)
//    println(RHS)
//    LHS should beIsomorphicTo(RHS)
//  }
//
//  val FCM20_19 = Dataset(source = Examples.FiniteCyclicMonoid(20, 19),
//    onObjects = Map(
//      "an element" -> List("a", "b", "c", "d")),
//    onMorphisms = Map(
//      "an element" --- "has as successor" --> "an element" -> Map(
//        "a" -> "c",
//        "b" -> "c",
//        "c" -> "d",
//        "d" -> "d")))
//
//  lazy val FCM20_19Times2L = Dataset(source = Examples.FiniteCyclicMonoid(20, 19),
//    onObjects = Map(
//      "an element" -> List("a1", "b1", "c1", "d1", "a2", "b2", "c2", "d2")),
//    onMorphisms = Map(
//      "an element" --- "has as successor" --> "an element" -> Map(
//        "a1" -> "a2",
//        "b1" -> "b2",
//        "c1" -> "c2",
//        "d1" -> "d2",
//        "a2" -> "c1",
//        "b2" -> "c1",
//        "c2" -> "d1",
//        "d2" -> "d1")))
//
//  val FCM20_19Times2R = Dataset(source = Examples.FiniteCyclicMonoid(20, 19),
//    onObjects = Map(
//      "an element" -> List("aa", "ab", "ac", "ad", "ba", "bb", "bc", "bd", "ca", "cb", "cc", "cd", "da", "db", "dc", "dd")),
//    onMorphisms = Map(
//      "an element" --- "has as successor" --> "an element" -> Map(
//        "aa" -> "ca",
//        "ab" -> "ca",
//        "ac" -> "da",
//        "ad" -> "da",
//        "ba" -> "cb",
//        "bb" -> "cb",
//        "bc" -> "db",
//        "bd" -> "db",
//        "ca" -> "cc",
//        "cb" -> "cc",
//        "cc" -> "dc",
//        "cd" -> "dc",
//        "da" -> "cd",
//        "db" -> "cd",
//        "dc" -> "dd",
//        "dd" -> "dd")))
//
//  // These tests run forever. What's going on?
//  //     "__*" should "provide a 'half-speed' FCM20_19" in {
//  //      println
//  //      println("Output from \"__* should provide a 'half-speed' FCM20_19\":")
//  //      
//  //      val X = FCM20_19
//  //      val T = Examples.TranslationFiniteCyclicMonoids(20,19,20,19,2);
//  //      val LHS = T.__*(X)
//  //      val RHS = FCM20_19Times2R
//  //      println(X)
//  //      println(LHS)
//  //      println(RHS)
//  //      LHS should beIsomorphicTo(RHS)
//  //   }      
//
//  val FCM7_6 = Dataset(source = Examples.FiniteCyclicMonoid(7, 6),
//    onObjects = Map(
//      "an element" -> List("a", "b", "c", "d")),
//    onMorphisms = Map(
//      "an element" --- "has as successor" --> "an element" -> Map(
//        "a" -> "c",
//        "b" -> "c",
//        "c" -> "d",
//        "d" -> "d")))
//
//  val FCM7_6Times2L = Dataset(source = Examples.FiniteCyclicMonoid(8, 6),
//    onObjects = Map(
//      "an element" -> List("a1", "b1", "c1", "d1", "a2", "b2", "c2", "d2")),
//    onMorphisms = Map(
//      "an element" --- "has as successor" --> "an element" -> Map(
//        "a1" -> "a2",
//        "b1" -> "b2",
//        "c1" -> "c2",
//        "d1" -> "d2",
//        "a2" -> "c1",
//        "b2" -> "c1",
//        "c2" -> "d1",
//        "d2" -> "d1")))
//
//  val FCM7_6Times2R = Dataset(source = Examples.FiniteCyclicMonoid(7, 6),
//    onObjects = Map(
//      "an element" -> List("aa", "ab", "ac", "ad", "ba", "bb", "bc", "bd", "ca", "cb", "cc", "cd", "da", "db", "dc", "dd")),
//    onMorphisms = Map(
//      "an element" --- "has as successor" --> "an element" -> Map(
//        "aa" -> "ca",
//        "ab" -> "ca",
//        "ac" -> "da",
//        "ad" -> "da",
//        "ba" -> "cb",
//        "bb" -> "cb",
//        "bc" -> "db",
//        "bd" -> "db",
//        "ca" -> "cc",
//        "cb" -> "cc",
//        "cc" -> "dc",
//        "cd" -> "dc",
//        "da" -> "cd",
//        "db" -> "cd",
//        "dc" -> "dd",
//        "dd" -> "dd")))
//
//  // These tests run forever. What's going on?
//  //     "__*" should "provide a 'half-speed' FCM20_19" in {
//  //      println
//  //      println("Output from \"__* should provide a 'half-speed' FCM20_19\":")
//  //      
//  //      val X = FCM20_19
//  //      val T = Examples.TranslationFiniteCyclicMonoids(20,19,20,19,2);
//  //      val LHS = T.__*(X)
//  //      val RHS = FCM20_19Times2R
//  //      println(X)
//  //      println(LHS)
//  //      println(RHS)
//  //      LHS should beIsomorphicTo(RHS)
//  //   }  
//
////  "__*" should "provide a 'half-speed' FCM7_6" in {// RUns forever I guess. Finding a smaller example below.
////    println
////    println("Output from \"__* should provide a 'half-speed' FCM7_6\":")
////
////    val X = FCM7_6
////    val T = Examples.TranslationFiniteCyclicMonoids(7, 6, 7, 6, 2);
////    val LHS = T.__*(X)
////    val RHS = FCM7_6Times2R
////    println(X)
////    println(LHS)
////    println(RHS)
////    LHS should beIsomorphicTo(RHS)
////  }
//
//
//
//        
//  
//  //    
//  //    "__!" should "provide a 'half-speed' FCM20_19" in {
//  //      println
//  //      println("Output from \"__! should provide a 'half-speed' FCM20_19\":")
//  //      
//  //      val X = FCM20_19
//  //      val T = Examples.TranslationFiniteCyclicMonoids(20,19,20,19,2);
//  //      val LHS = T.__!(X)
//  //      val RHS = FCM20_19Times2L
//  //      println(X)
//  //      println(LHS)
//  //      println(RHS)
//  //      LHS should beIsomorphicTo(RHS)
//  //   }  
//  
//          // Scott: I've commented this out again 2012-01-11, as it still runs forever. 
////      "__!" should "provide a 'half-speed' FCM20_18" in {
////        println
////        println("Output from \"__! should provide a 'half-speed' FCM20_18\":")
////        
////        val X = FCM20_18
////        val T = Examples.TranslationFiniteCyclicMonoids(20,18,20,18,2);
////        val LHS = T.__!(X)
////        val RHS = FCM20_18Times2L
////        println(X)
////        println(LHS)
////        println(RHS)
////        LHS should beIsomorphicTo(RHS)
////     }       
//
//  
//
//  //   "fullSubcategorySpannedBy" should "take Chains to Chains" in {//TODO Make a working fullSubcategorySpannedBy method.
//  //	   val X = Examples.Chain(5)
//  //	   val RHS = Examples.Chain(3)
//  //	   val LHS = X.fullSubcategorySpannedBy(List("0","1","2","3")/*The fact that these are boxes should be inherent?*/)
//  //	   LHS should equal(RHS)
//  //   }
//    
//
//}