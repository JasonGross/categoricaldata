package net.metaphor

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.metaphor.api.Ontology
import net.metaphor.api.Path
import net.metaphor.api.FiniteTarget
import net.metaphor.api.FiniteMorphisms
import net.metaphor.api.Ontologies
import net.metaphor.examples.Examples
/*
 * Currently no expectation that this code compiles.
 */

@RunWith(classOf[JUnitRunner])
class Test extends FlatSpec with ShouldMatchers {
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

 

  def Ord(n: Int) = Ontology(
    objects = for (i <- 0 to n) yield "V" + i.toString, //David added the ".toString" here. Correct? // Yes ---S
    arrows = for (i <- 0 to n - 1) yield {
      ("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString)
    }).assertAcyclic

  val Domain = Translation(
    source = Ord(0),
    target = Ord(1),
    onObjects = Map("V0" -> "V0"),
    onMorphisms = Map())

  val Codomain = Translation(
    source = Ord(0),
    target = Ord(1),
    onObjects = Map("V0" -> "V1"),
    onMorphisms = Map())

  //  def Skip (n : Int, k : Int) = functor (
  //      source = Ord(n),
  //      target = Ord(n+1),
  //      onObjects =
  //          (for (i <- 0 to k-1) yield ("V" + i.toString) -> ("V" + i.toString)).toMap ++
  //          (for (i <- k to n) yield ("V" + i.toString) -> ("V" + (i + 1).toString)).toMap,
  //      onMorphisms = 
  //          (for (i <- 0 to k-1) yield {
  //          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
  //          	->
  //          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))}).toMap ++
  //          (("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString))
  //          ->
  //          (("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString) --- ("E" + (k + 1).toString + (k + 2).toString) --> ("V" + (k + 2).toString))
  //          (for (i <- k+1 to n-1) yield {
  //            (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
  //          	->
  //          	(("V" + (i+1).toString) --- ("E" + (i + 1).toString + (i + 2).toString) --> ("V" + (i + 2).toString))}).toMap)

  //  def Coface(n: Int, k: Int) = Skip (n,k)
  //
  //  def Duplicate(n : Int, k : Int) = functor (
  //      source = Ord(n),
  //      target = Ord(n-1),
  //      onObjects = 
  //          (for (i <- 0 to k) yield ("V" + i.toString) -> ("V" + i.toString)).toMap ++
  //          (for (i <- k+1 to n) yield ("V" + i.toString) -> ("V" + (i - 1).toString)).toMap,
  //      onMorphisms = Map (
  //          for (i <- 0 to k-1) yield {
  //          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
  //          	->
  //          	(("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))},
  //          (("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString))
  //          ->
  //          ("V" + k.toString),
  //          for (i <- k+1 to n-1) yield {
  //            (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
  //          	->
  //          	(("V" + (i - 1).toString) --- ("E" + (i - 1).toString + i.toString) --> ("V" + i.toString))}))
  //  
  //  def Codegeneracy(n : Int, k : Int) = Duplicate (n,k)
  //  

  val Compose = Translation(
    source = Ord(1),
    target = Ord(2),
    onObjects = Map(
      "V0" -> "V0",
      "V1" -> "V2"),
    onMorphisms = Map(
      ("V0" --- "E01" --> "V1") -> ("V0" --- "E01" --> "V1" --- "E12" --> "V2")))

  val TerminalCategory = Ord(0)

  // Scott says: I fixed this up, but we need to talk about this in some detail!
  // David says: Yes, we should. I don't understand what's going on with Path(_, List(a)).
  
  def TerminalFunctor(c: Ontology) = Translation(
    source = c,
    target = TerminalCategory,
    onObjects = (for (b <- c.objects) yield (b.name -> "V0")).toMap,
    onMorphisms = (for (Path(_, List(a)) <- c.allGenerators) yield (a.source.name --- a.name --> a.target.name) -> stringAsPath("V0")).toMap //
    )

//  def TerminalDataset(c: Ontology) = Dataset(
//    source = c,
//    onObjects = (for (b <- c.objects) yield (b.name -> List("witness" + b.name))).toMap,
//    onMorphisms = (for (a <- c.arrows) yield {
//      Map((a.source.name --- a.name --> a.target.name) ->
//        Map("witness" + a.source.name -> "witness" + a.target.name))
//    }))

  val InitialCategory = Ontology(
    objects = List(),
    arrows = List())

  def InitialFunctor(c: Ontology with Ontologies.Finite) = Translation(
    source = InitialCategory,
    target = c,
    onObjects = Map(),
    onMorphisms = Map())

  //  def InitialDataset (c : Ontology) = Dataset(
  //      source = c,
  //      onObjects = for (b <- c.boxes) yield Map (b.name -> List ()),
  //      onMorphisms = for (a <- c.arrows) yield Map ((a.source.name --- a.name --> a.target.name) -> Map ()))
  //      

//    def OppositeOntology (c: Ontology) = Ontology ( 
//		  objects = c.objects
//		  arrows = for (a <- c.arrows) yield {a.target --- a.name --> a.source} 
//		  relations = ???)
		  
//def OppositeFunctor (f:Functor) = Functor( 
//    source = OppositeOntology(f.source)
//    target = OppositeOntology(f.target)
//    onObjects = f.onObjects
//    onMorphisms = for (???))
    
  val SourceFunction = Translation(
    source = Ord(1),
    target = Examples.Grph,
    onObjects = Map(
      "V0" -> "an edge",
      "V1" -> "a vertex"),
    onMorphisms = Map(
      ("V0" --- "E01" --> "V1") -> ("an edge" --- "has as source" --> "a vertex")))

  val TargetFunction = Translation(
    source = Ord(1),
    target = Examples.Grph,
    onObjects = Map(
      "V0" -> "an edge",
      "V1" -> "a vertex"),
    onMorphisms = Map(
      ("V0" --- "E01" --> "V1") -> ("an edge" --- "has as target" --> "a vertex")))

  val DiscreteDynamicalSystem = Ontology(
    objects = List("an element"),
    arrows = List("an element" --- "has as successor" --> "an element"))

  val DavidsFunkyDiscreteDynamicalSystem = Dataset(source = DiscreteDynamicalSystem,
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

  val ReverseGraph = Translation(
    source = Examples.Grph,
    target = Examples.Grph,
    onObjects = Map(
      "an edge" -> "an edge",
      "a vertex" -> "a vertex"),
    onMorphisms = Map(
      ("an edge" --- "has as source" --> "a vertex") -> ("an edge" --- "has as target" --> "a vertex"),
      ("an edge" --- "has as target" --> "a vertex") -> ("an edge" --- "has as source" --> "a vertex")))

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

        // FIXME (Scott) Allow translations with infinite targets.
  // Hmm, I've commented this out for now, as I'm only allowing Translations with finite targets.
//  val GraphToDiscreteDynamicalSystem1 = Translation(
//    source = Grph,
//    target = DiscreteDynamicalSystem,
//    onObjects = Map(
//      "an edge" -> "an element",
//      "a vertex" -> "an element"),
//    onMorphisms = Map(
//      ("an edge" --- "has as source" --> "a vertex") -> ("an element".identity),
//      ("an edge" --- "has as target" --> "a vertex") -> ("an element" --- "has as successor" --> "an element")))

      // TODO (David) this is broken; there's no object in DiscreteDynamicalSystem called "a vertex"
//  val GraphToDiscreteDynamicalSystem2 = Translation(
//    source = Grph,
//    target = DiscreteDynamicalSystem,
//    onObjects = Map(
//      "an edge" -> "an element",
//      "a vertex" -> "an element"),
//    onMorphisms = Map(
//      ("an edge" --- "has as source" --> "a vertex") -> ("an element" --- "has as successor" --> "an element"),
//      ("an edge" --- "has as target" --> "a vertex") -> ("an element".identity)))

  val IntegersMod2Group = Ontology(
    objects = List("an element"),
    arrows = List("an element" --- "is married to" --> "an element"),
    relations = List(
      ("an element" --- "is married to" --> "an element" --- "is married to" --> "an element")
        ===
        ("an element")))

        
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

  val Isomorphism = Ontology(

    objects = List("0", "1"),
    arrows = List(
      "0" --- "E01" --> "1",
      "1" --- "E10" --> "0"),
    relations = List(
      (("0" --- "E01" --> "1" --- "E10" --> "0")
        ===
        ("0")),
      (("1" --- "E10" --> "0" --- "E01" --> "1")
        ===
        ("1"))))

  val PointedSet = Ontology(
    objects = List ("an element", "a pointed set"),
    arrows = List (
      "an element" --- "is in" --> "a pointed set",
      "a pointed set" --- "has as chosen" --> "an element"),
    relations = List (
      ("a pointed set" --- "has as chosen" --> "an element" --- "is in" --> "a pointed set")
        ===
        ("a pointed set")))
  
  val E2 = Ontology( 
	objects = List ("0", "1"),
	arrows = List(
      "0" --- "E01" --> "1",
      "1" --- "E10" --> "0")
    ).assertGraph
  
  val E2ToPointedSet = Translation (
    source = E2,
    target = PointedSet,
    onObjects = Map (
        "0" -> "an element", 
        "1" -> "a pointed set"),
    onMorphisms = Map (
      ("0" --- "E01" --> "1") -> ("an element" --- "is in" --> "a pointed set"),
      ("1" --- "E10" --> "0") -> ("a pointed set" --- "has as chosen" --> "an element")
    )
  )
  
  val PointedSetToIsomorphism = Translation (
	source = PointedSet,
    target = Isomorphism,
    onObjects = Map (
        "an element" -> "0", 
        "a pointed set" -> "1"),
    onMorphisms = Map (
      ("an element" --- "is in" --> "a pointed set") -> ("0" --- "E01" --> "1"),
      ("a pointed set" --- "has as chosen" --> "an element") -> ("1" --- "E10" --> "0")
    )
  )    
    
  val FunkyE2Dataset = Dataset ( 
	source = E2,
	onObjects = Map ( 
	    "0" -> List ("a","b","c","d"),
	    "1" -> List ("1","2","3")),
	onMorphisms = Map ( 
		"0" --- "E01" --> "1" -> Map (  
		    "a" -> "1",
		    "b" -> "1",
		    "c" -> "2",
		    "d" -> "3"),
      "1" --- "E10" --> "0" -> Map ( 
          "1" -> "a",
          "2" -> "b",
          "3" -> "d")
	)
  )
  
  val E2ToPointedSetRPushFunky = Dataset (
    source = PointedSet,
    onObjects = Map (
        "an element" -> List("a1a","b1a","d3d"), 
        "a pointed set" -> List ("a1","d3")),
    onMorphisms = Map ( 
       ("an element" --- "is in" --> "a pointed set") -> Map ( 
           "a1a" -> "a1",
           "ba1" -> "a1",
           "d3d" -> "d3"),
       ("a pointed set" --- "has as chosen" --> "an element") -> Map (
           "a1" -> "a1a",
           "d3" -> "d3d")
    )
  )
  
 

  
  
  val DavidsFunkyFunction = Dataset(source = Ord(1),
    onObjects = Map(
      "V0" -> List("David", "Scott", "UC Berkeley", "MIT"),
      "V1" -> List("1978", "Scott's birthyear", "1868", "1861")),
    onMorphisms = Map(
      "V0" --- "E01" --> "V1" -> Map(
        "David" -> "1978",
        "Scott" -> "Scott's birthyear",
        "UC Berkeley" -> "1868",
        "MIT" -> "1861")))

  val DavidsFunkySet1 = Dataset(source = Ord(0),
    onObjects = Map(
      "V0" -> List("David", "Scott", "UC Berkeley", "MIT")),
    onMorphisms = Map())

  val DavidsFunkySet2 = Dataset(source = Ord(0),
    onObjects = Map(
      "V0" -> List("1978", "Scott's birthyear", "1868", "1861")),
    onMorphisms = Map())

  val Drawers = Dataset(Ord(1),
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
    Domain.^*(DavidsFunkyFunction) should equal(DavidsFunkySet1)
  }

  "pullback" should "work with the codomain inclusion" in {
    Codomain.^*(DavidsFunkyFunction) should equal(DavidsFunkySet2)
  }
  
   "pushforward" should "work nicely with the map from E2 to PointedSet" in {
     E2ToPointedSet.__*(FunkyE2Dataset).isIsomorphicTo(E2ToPointedSetRPushFunky) should equal(true)
  
  }

//  "pullback" should "work with the GraphToDiscreteDynamicalSystem1 functor" in {
//    GraphToDiscreteDynamicalSystem1.^*(DavidsFunkyDiscreteDynamicalSystem) should equal(GraphFromDavidsFunkyDiscreteDynamicalSystem)
//  }

  // Unfortunately it seems that _* and _! aren't allowed method names. I've gone with __* and __! for now.

  "pushforward" should "work (1)" in {
    TerminalFunctor(Ord(1)).__*(DavidsFunkyFunction) should equal(DavidsFunkySet1)
  }

  "shriek" should "work (1)" in {
    TerminalFunctor(Ord(1)).__!(DavidsFunkyFunction) should equal(DavidsFunkySet2)
    
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
    ReverseGraph.^*(DavidsFunkyGraph) should equal(DavidsFunkyGraphReversed)
  }

  "pushforward" should "work (3)" in {
    ReverseGraph.__*(DavidsFunkyGraph) should equal(DavidsFunkyGraphReversed)
  }

  "shriek" should "work (2)" in {
    ReverseGraph.__!(DavidsFunkyGraph) should equal(DavidsFunkyGraphReversed)
  }

}