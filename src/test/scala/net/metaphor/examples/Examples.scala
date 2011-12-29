package net.metaphor.examples
import net.metaphor.api.Ontology
import net.metaphor.api.Ontologies

object Examples {

  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._
  
  
  val Grph = Ontology(
    objects = List("an edge", "a vertex"),
    arrows = List(
      "an edge" --- "has as source" --> "a vertex",
      "an edge" --- "has as target" --> "a vertex")).assertAcyclic.assertFree 

  val TerminalGraph = Dataset(source = Grph,
    onObjects = Map(
      "an edge" -> List("+1"),
      "a vertex" -> List("N")),
    onMorphisms = Map(
      ("an edge" --- "has as source" --> "a vertex") -> Map("+1" -> "N"),
      ("an edge" --- "has as target" --> "a vertex") -> Map("+1" -> "N")))

  val TerminalBigraph = Dataset(source = Grph,
    onObjects = Map(
      "an edge" -> List("input", "output"),
      "a vertex" -> List("species", "transition")),
    onMorphisms = Map(
      ("an edge" --- "has as source" --> "a vertex") -> Map(
        "input" -> "species",
        "output" -> "transition"),
      ("an edge" --- "has as target" --> "a vertex") -> Map(
        "input" -> "transition",
        "output" -> "species")))

  val InitialGraph = Dataset(source = Grph,
    onObjects = Map(
      "an edge" -> List(),
      "a vertex" -> List()),
    onMorphisms = Map(
      ("an edge" --- "has as source" --> "a vertex") -> Map(),
      ("an edge" --- "has as target" --> "a vertex") -> Map()))
  
   def Ord(n: Int) = Ontology(
    objects = for (i <- 0 to n) yield "V" + i.toString, //David added the ".toString" here. Correct? // Yes ---S
    arrows = for (i <- 0 to n - 1) yield {
      ("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString)
    }).assertAcyclic

  
  val Domain = Translation(
    source = Examples.Ord(0),
    target = Examples.Ord(1),
    onObjects = Map("V0" -> "V0"),
    onMorphisms = Map())

  val Codomain = Translation(
    source = Examples.Ord(0),
    target = Examples.Ord(1),
    onObjects = Map("V0" -> "V1"),
    onMorphisms = Map())

  //  def Skip (n : Int, k : Int) = functor (
  //      source = Examples.Ord(n),
  //      target = Examples.Ord(n+1),
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
  //      source = Examples.Ord(n),
  //      target = Examples.Ord(n-1),
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
    source = Examples.Ord(1),
    target = Examples.Ord(2),
    onObjects = Map(
      "V0" -> "V0",
      "V1" -> "V2"),
    onMorphisms = Map(
      ("V0" --- "E01" --> "V1") -> ("V0" --- "E01" --> "V1" --- "E12" --> "V2")))

  val TerminalCategory = Examples.Ord(0)

  // Scott says: I fixed this up, but we need to talk about this in some detail!
  // David says: Yes, we should. I don't understand what's going on with Path(_, List(a)).
  
  def TerminalFunctor(c: Ontology) = Translation(
    source = c,
    target = TerminalCategory,
    onObjects = (for (b <- c.objects) yield (b.name -> "V0")).toMap,
    onMorphisms = (for (a <- c.allGenerators) yield (a.source.name --- a.name --> a.target.name) -> stringAsPath("V0")).toMap //
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
    source = Examples.Ord(1),
    target = Examples.Grph,
    onObjects = Map(
      "V0" -> "an edge",
      "V1" -> "a vertex"),
    onMorphisms = Map(
      ("V0" --- "E01" --> "V1") -> ("an edge" --- "has as source" --> "a vertex")))

  val TargetFunction = Translation(
    source = Examples.Ord(1),
    target = Examples.Grph,
    onObjects = Map(
      "V0" -> "an edge",
      "V1" -> "a vertex"),
    onMorphisms = Map(
      ("V0" --- "E01" --> "V1") -> ("an edge" --- "has as target" --> "a vertex")))

  val DiscreteDynamicalSystem = Ontology(
    objects = List("an element"),
    arrows = List("an element" --- "has as successor" --> "an element"))

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
        ("1")))).assertFinite

  val PointedSet = Ontology(
    objects = List ("an element", "a pointed set"),
    arrows = List (
      "an element" --- "is in" --> "a pointed set",
      "a pointed set" --- "has as chosen" --> "an element"),
    relations = List (
      ("a pointed set" --- "has as chosen" --> "an element" --- "is in" --> "a pointed set")
        ===
        ("a pointed set"))).assertFinite
  
  val E2 = Ontology( 
	objects = List ("0", "1"),
	arrows = List(
      "0" --- "E01" --> "1",
      "1" --- "E10" --> "0")
    ).assertFree
  
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

  
  
  
}