package net.metaphor

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.metaphor.api.Ontology
import net.metaphor.api.FiniteTarget
import net.metaphor.api.FiniteMorphisms
import net.metaphor.api.Ontologies
import net.metaphor.examples.Examples
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
/*
 * This should always compile when checked in.
 */

@RunWith(classOf[JUnitRunner])
class Test extends FlatSpec with ShouldMatchers {
  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._

  class IsomorphismMatcher(right: Ontology#Dataset) extends Matcher[Ontology#Dataset] {
    def apply(left: Ontology#Dataset) = {
      MatchResult(
        left.isIsomorphicTo(right),
        "The data sets are not isomorphic",
        "The data sets are isomorphic")
    }
  }

  def beIsomorphicTo(d: Ontology#Dataset) = new IsomorphismMatcher(d)

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

  //TODO: Explain to David why FiniteCyclicMonoid is no good:
  //     def FiniteCyclicMonoid (n : Int, k : Int) = Ontology (//should have k < n. When k = 0, this is the cyclic group of order n.
  //         objects = List ("an element"),
  //         arrows = List ("an element" --- "has as successor" --> "an element"),
  //         relations = List (
  //             (for (i <- 1 to n) yield {"an element" --- "has as successor" -->} + "an element")
  //             ===
  //             (for (i <- 1 to k) yield {"an element" --- "has as successor" -->} + "an element")))
  //             
  //     def TerminalCategoryToFiniteCyclicMonoid (n : Int, k : Int) = Translation(
  //         source = TerminalCategory,
  //         target = FiniteCyclicMonoid(n, k),
  //         onObjects = Map ("V0" -> "an element"),
  //         onMorphisms = Map ())
  //             
  //     val DavidsFunkyFiniteCyclicMonoid = Dataset (
  //         source = FiniteCyclicMonoid(2,1),
  //         onObjects = Map ("an element" -> List ("David","Scott","UC Berkeley", "MIT","succDavid","succScott","succUC Berkeley", "succMIT")),
  //         onMorphisms = Map ("an element" --- "has as successor" --> "an element" -> Map (
  //             "David" -> "succDavid",
  //             "Scott" -> "succScott",
  //             "UC Berkeley" -> "succUC Berkeley",
  //             "MIT" -> "succMIT",
  //             "succDavid" -> "succDavid",
  //             "succScott" -> "succScott",
  //             "succUC Berkeley" -> "succUC Berkeley", 
  //             "succMIT" -> "succMIT")))
  //             
  //

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

  val E2ToPointedSetRPushFunky = Dataset(
    source = Examples.PointedSet,
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

<<<<<<< local
  val OneTwoThreePointed = Dataset(
    source = Examples.PointedSet,
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

=======
  val SixElementsIso = Dataset (
    source = Examples.Isomorphism,  
    onObjects = Map (
        "0" -> List ("a1", "b1", "b2", "c1", "c2", "c3"),
        "1" -> List ("a1", "b1", "b2", "c1", "c2", "c3")),
    onMorphisms = Map (
        ("0" --- "E01" --> "1") -> Map (
        	"a1" -> "a1",
           	"b1" -> "b1",
           	"b2" -> "b2",
           	"c1" -> "c1",
           	"c2" -> "c2",
           	"c3" -> "c3"),
        ("1" --- "E10" --> "0") -> Map (
        	"a1" -> "a1",
        	"b1" -> "b1",
        	"b2" -> "b2",
        	"c1" -> "c1",
        	"c2" -> "c2",
        	"c3" -> "c3")
     )
  )
  
>>>>>>> other
  val ThreeElementsIso = Dataset(
<<<<<<< local
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

=======
  	source = Examples.Isomorphism,
  	onObjects = Map (
        "0" -> List ("a", "b", "c"),
        "1" -> List ("a", "b", "c")),
    onMorphisms = Map (
        ("0" --- "E01" --> "1") -> Map (
        	"a" -> "a",
           	"b" -> "b",
           	"c" -> "c"),
        ("1" --- "E10" --> "0") -> Map (
        	"a" -> "a",
           	"b" -> "b",
           	"c" -> "c")
     )
  )
  
  
  
  	
>>>>>>> other
  val DavidsFunkyFunction = Dataset(source = Examples.Ord(1),
    onObjects = Map(
      "V0" -> List("David", "Scott", "UC Berkeley", "MIT"),
      "V1" -> List("1978", "Scott's birthyear", "1868", "1861")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "David" -> "1978",
        "Scott" -> "Scott's birthyear",
        "UC Berkeley" -> "1868",
        "MIT" -> "1861")))

  val DavidsFunkySet1 = Dataset(source = Examples.Ord(0),
    onObjects = Map(
      "V0" -> List("David", "Scott", "UC Berkeley", "MIT")),
    onMorphisms = Map())

  val DavidsFunkySet2 = Dataset(source = Examples.Ord(0),
    onObjects = Map(
      "V0" -> List("1978", "Scott's birthyear", "1868", "1861")),
    onMorphisms = Map())

  val Drawers = Dataset(Examples.Ord(1),
    onObjects = Map(
      "V0" -> List("Item 1", "Item 2", "Item 3", "Item 4"),
      "V1" -> List("Top Drawer", "Bottom Drawer")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "Item 1" -> "Top Drawer",
        "Item 2" -> "Bottom Drawer",
        "Item 3" -> "Top Drawer",
        "Item 4" -> "Top Drawer")))

  "pullback" should "work with the domain inclusion" in {
    Examples.Domain.^*(DavidsFunkyFunction) should equal(DavidsFunkySet1)
  }

  "pullback" should "work with the codomain inclusion" in {
    Examples.Codomain.^*(DavidsFunkyFunction) should equal(DavidsFunkySet2)
  }

  "pushforward" should "work nicely with the map from E2 to PointedSet" in {
    Examples.E2ToPointedSet.__*(FunkyE2Dataset) should beIsomorphicTo(E2ToPointedSetRPushFunky)
  }

  // TODO waiting on translations with infinite targets
  //    "pullback" should "work with the GraphToDiscreteDynamicalSystem1 functor" in {
  //      GraphToDiscreteDynamicalSystem1.^*(DavidsFunkyDiscreteDynamicalSystem) should equal(GraphFromDavidsFunkyDiscreteDynamicalSystem)
  //    }

  "pushforward" should "work (1)" in {
    val pushforward = Examples.TerminalFunctor(Examples.Ord(1)).__*(DavidsFunkyFunction)
    pushforward.isIsomorphicTo(DavidsFunkySet1) should equal(true)
  }

  "shriek" should "work (1)" in {
    val shriek = Examples.TerminalFunctor(Examples.Ord(1)).__!(DavidsFunkyFunction)
    shriek.isIsomorphicTo(DavidsFunkySet2) should equal(true)
  }

  //   "shriek" should "work (1)" in {
  //	   TerminalCategoryToFiniteCyclicMonoid(2,1).__!(DavidsFunkySet1) should equal (DavidsFunkyFiniteCyclicMonoid)
  //   }
  //
  //  "pushforward" should "work (2)" in {
  //    GraphToDiscreteDynamicalSystem1.__*(DavidsFunkyGraph).isIsomorphicTo(DavidsFunkyDiscreteDynamicalSystem) should equal(true)
  //  }

  // Let 0:C-->Set be the initial dataset and 1:C-->Set the terminal dataset.
  // For any functor F:C-->D, we have F^*(0)=0, F^*(1)=1, F_!(0)=0, F_*(1)=1.
  // I chose some random functor from above, TerminalCategoryToFiniteCyclicMonoid(10,7), and did the calculation there.

  //   "pullback" should "preserve the initial dataset" in {
  //	   TerminalCategoryToFiniteCyclicMonoid(10,7).^*(InitialDataset(FiniteCyclicMonoid)) 
  //	   should equal 
  //	   (InitialDataset(TerminalCategory))
  //   }
  //   
  //   "pullback" should "preserve the terminal dataset" in {
  //	   TerminalCategoryToFiniteCyclicMonoid(10,7).^*(TerminalDataset(FiniteCyclicMonoid)) 
  //	   should equal 
  //	   (TerminalDataset(TerminalCategory))
  //   }
  //   
  //   "pushforward" should "preserve the terminal dataset" in {
  //	   TerminalCategoryToFiniteCyclicMonoid(10,7).__*(TerminalDataset(TerminalCategory)) 
  //	   should equal 
  //	   (TerminalDataset(FiniteCyclicMonoid(10,7)))
  //   }
  //   
  //   "shriek" should "preserve the initialdataset" in {
  //	   TerminalCategoryToFiniteCyclicMonoid(10,7).__!(InitialDataset(TerminalCategory)) 
  //	   should equal 
  //	   (InitialDataset(FiniteCyclicMonoid(10,7)))
  //   }

  "pullback" should "work (4)" in {
    Examples.ReverseGraph.^*(DavidsFunkyGraph) should equal(DavidsFunkyGraphReversed)
  }

  "pushforward" should "work (3)" in {
<<<<<<< local
    val LHS = ReverseGraph.__*(DavidsFunkyGraph)
    val RHS = DavidsFunkyGraphReversed
    println(LHS)
    println(RHS)
    LHS should beIsomorphicTo(RHS)
=======
    Examples.ReverseGraph.__*(DavidsFunkyGraph) should equal(DavidsFunkyGraphReversed)
>>>>>>> other
  }

  "shriek" should "work (2)" in {
<<<<<<< local
    val LHS = ReverseGraph.__!(DavidsFunkyGraph)
    val RHS = DavidsFunkyGraphReversed
    println(LHS)
    println(RHS)
    LHS should beIsomorphicTo(RHS)
=======
    Examples.ReverseGraph.__!(DavidsFunkyGraph) should equal(DavidsFunkyGraphReversed)
>>>>>>> other
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
  "colimit and shriek" should "agree for terminal functors" in {
    //For any category C, and any dataset D:C-->Set, we should have colim(D)=TerminalFunctor(C).__!(D)
  }
  "limit and pushforward" should "agree for terminal functors" in {
    //For any category C, and any dataset D:C-->Set, we should have lim(D)=TerminalFunctor(C).__*(D)
  }
  "pushforward" should "work with PointedSetToIsomorphism" in {
<<<<<<< local
    Examples.PointedSetToIsomorphism.__*(OneTwoThreePointed) should beIsomorphicTo(SixElementsIso)
=======
    Examples.PointedSetToIsomorphism.__*(OneTwoThreePointed) should equal(SixElementsIso)
>>>>>>> other
  }

  "shriek" should "work with PointedSetToIsomorphism" in {
<<<<<<< local
    Examples.PointedSetToIsomorphism.__!(OneTwoThreePointed) should beIsomorphicTo(ThreeElementsIso)
  }
=======
	  Examples.PointedSetToIsomorphism.__!(OneTwoThreePointed) should equal(ThreeElementsIso)
  }  
>>>>>>> other
}