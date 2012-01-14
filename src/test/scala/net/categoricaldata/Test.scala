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
/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class Test extends FlatSpec with ShouldMatchers with CustomMatchers {
  // NOTE to use the DSL, you need this line:
  import net.categoricaldata.dsl.Sentences._

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

  //   "__!" should "work (1)" in {
  //	   TerminalCategoryToFiniteCyclicMonoid(2,1).__!(DavidsFunkySet1) should equal (DavidsFunkyFiniteCyclicMonoid)
  //   }
  //
  //  "__*" should "work (2)" in {
  //    GraphToDiscreteDynamicalSystem1.__*(DavidsFunkyGraph).isIsomorphicTo(DavidsFunkyDiscreteDynamicalSystem) should equal(true)
  //  }

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

  // TODO (Scott): Can the following two tests be made "generic" in the way I want them to? See comments.
  // Scott: sure, if you have some supply of datasets, you could write
  /*
   * for(dataset <- dataset; C = dataset.source) {
   *    val T = TerminalFunctor(C)
   *    val x = T.target.allObjects.head 									// the target only has one object, so this hack to get it is okay.
   * 	dataset.colimitSet should equal(TerminalFunctor(C).__!(dataset)(x))
   * }
   */
  "colimit and __!" should "agree for terminal functors" in {
    //For any category C, and any dataset D:C-->Set, we should have colim(D)=TerminalFunctor(C).__!(D)
    for (
      dataset <- List[Ontology#Dataset]()
    ) {
      val C = dataset.source
      val T = Ontologies.morphismToTerminalObject(C)
      val x = T.target.objects.head // the target only has one object, so this hack to get it is okay.
      dataset.colimitSet should equal(T.__!(dataset)(x))
    }
  }
  "limit and __*" should "agree for terminal functors" in {
    //For any category C, and any dataset D:C-->Set, we should have lim(D)=TerminalFunctor(C).__*(D)
  }

  "__!" should "work with PointedSetsToIsomorphism" in {
    //    println
    //    println("Output from \"__! should take this retraction and return two sets isomorphic to its total space.\":")
    //    println
    val X = OneTwoThreePointed
    val LHS = ThreeElementsIso
    val RHS = Examples.PointedSetsToIsomorphism.__!(X)
    //    println("Original retraction: "); println(X);println
    //    println("Expected isomorphism: "); println (LHS);println
    //    println("Left pushforward of original retraction: ");println (RHS)
    LHS should beIsomorphicTo(RHS)
  }

  val SaturatedCommutativeTriangle = {
    val ArrowsList = List(
      "V0" --- "E00" --> "V0",
      "V0" --- "E01" --> "V1",
      "V0" --- "E02" --> "V2",
      "V1" --- "E11" --> "V1",
      "V1" --- "E12" --> "V2",
      "V2" --- "E22" --> "V2")
    val RelationsId0 = List(
      ("V0" --- "E00" --> "V0" --- "E00" --> "V0") === ("V0" --- "E00" --> "V0"),
      ("V0" --- "E00" --> "V0" --- "E01" --> "V1") === ("V0" --- "E01" --> "V1"),
      ("V0" --- "E00" --> "V0" --- "E02" --> "V2") === ("V0" --- "E02" --> "V2"))
    val RelationsId1 = List(
      ("V0" --- "E01" --> "V1" --- "E11" --> "V1") === ("V0" --- "E01" --> "V1"),
      ("V1" --- "E11" --> "V1" --- "E11" --> "V1") === ("V1" --- "E11" --> "V1"),
      ("V1" --- "E11" --> "V1" --- "E12" --> "V2") === ("V1" --- "E12" --> "V2"))
    val RelationsId2 = List(
      ("V0" --- "E02" --> "V2" --- "E22" --> "V2") === ("V0" --- "E02" --> "V2"),
      ("V1" --- "E12" --> "V2" --- "E22" --> "V2") === ("V1" --- "E12" --> "V2"),
      ("V2" --- "E22" --> "V2" --- "E22" --> "V2") === ("V2" --- "E22" --> "V2"))
    val Comp012 = List(
      ("V0" --- "E01" --> "V1" --- "E12" --> "V2") === ("V0" --- "E02" --> "V2"))

    Ontology(
      objects = List("V0", "V1", "V2"),
      arrows = ArrowsList,
      relations = List.concat(RelationsId0, RelationsId1, RelationsId2, Comp012))
  }

  //  "BeIsomorphicAsCategoriesTo" should "see the iso between saturated commutative triangle and Chain2" in { //TODO (Scott) Is this test in the right place? Do we have BeIsomorphicAsCategoriesTo?
  //    Examples.Chain(2) should BeIsomorphicAsCategoriesTo(SaturatedCommutativeTriangle)
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

  //      val DDSTimeLapse2 = Translation ( //TODO: This was commented out because the categories are infinite. But it should work.
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

        
 
        
//The following was commented out because it doesn't work, and I can find a simpler example. I'll put it back in when FCM4_2 works, as an example of a "Big computation".        
        
//  val FCM20_18 = Dataset(source = Examples.FiniteCyclicMonoid(20, 18),
//    onObjects = Map(
//      "an element" -> List("a", "b", "c", "d")),
//    onMorphisms = Map(
//      "an element" --- "has as successor" --> "an element" -> Map(
//        "a" -> "c",
//        "b" -> "c",
//        "c" -> "d",
//        "d" -> "d")))
//
//  
//  val FCM20_18Times2L = Dataset(source = Examples.FiniteCyclicMonoid(20, 18),
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
//  val FCM20_18Times2R = Dataset(source = Examples.FiniteCyclicMonoid(20, 18),
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
        
  val FCM4_3 = Dataset(source = Examples.FiniteCyclicMonoid(4, 3),
    onObjects = Map(
      "an element" -> List("a", "b")),
    onMorphisms = Map(
      "an element" --- "has as successor" --> "an element" -> Map(
        "a" -> "b",
        "b" -> "b")))

  
  val FCM4_3Times2LToFCM5_3 = Dataset(source = Examples.FiniteCyclicMonoid(5, 3),
    onObjects = Map(
      "an element" -> List("a1", "b1", "a2", "b2")),
    onMorphisms = Map(
      "an element" --- "has as successor" --> "an element" -> Map(
        "a1" -> "a2",
        "b1" -> "b2",
        "a2" -> "b1",
        "b2" -> "b1")))

  val FCM4_3Times2RToFCM5_3 = Dataset(source = Examples.FiniteCyclicMonoid(5, 3),
    onObjects = Map(
      "an element" -> List("aa", "ab", "ba", "bb")),
    onMorphisms = Map(
      "an element" --- "has as successor" --> "an element" -> Map(
        "aa" -> "ba",
        "ab" -> "ba",
        "ba" -> "bb",
        "bb" -> "bb")))

    "__*" should "provide a 'half-speed' FCM-thing" in {
//        println
//        println("Output from \"__* should take a dataset on FCM4_3, cut its speed in half, and return a dataset on FCM5_3\":")
        
        val X = FCM4_3
        val LHS = Examples.TranslationFiniteCyclicMonoids(4,3,5,3,2)__*(X)
        val RHS = FCM4_3Times2RToFCM5_3
//        println(X)
//        println(LHS)
//        println(RHS)
        LHS should beIsomorphicTo(RHS)
     }      
      
      "__!" should "provide a 'half-speed' FCM-thing" in {
//        println
//        println("Output from \"__! should take a dataset on FCM4_3, cut its speed in half, and return a dataset on FCM5_3\":")
        
        val X = FCM4_3
        val LHS = Examples.TranslationFiniteCyclicMonoids(4,3,5,3,2).__!(X)
        val RHS = FCM4_3Times2LToFCM5_3
//        println(X)
//        println(LHS)
//        println(RHS)
        LHS should beIsomorphicTo(RHS)
     }      

  //TODO (David): Make a test for commutative diagram of ontologies. 
  //Let A=Span, B=non-commuting triangle, C=Chain(1), and D=commuting triangle. 
  //A-->B gives two paths start to end, A-->C is obvious, B-->D is obvious, C-->D is hypotenuse. 
  //Check that this diagram commutes.
}