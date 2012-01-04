package net.metaphor.examples
import net.metaphor.api.Ontology
import net.metaphor.api.Ontologies

object Examples {

  // NOTE to use the DSL, you need this line:
  import net.metaphor.dsl.Sentences._
  
  def DiscreteCategoryOn(n:Int) = Ontology(
     objects = for (i <- 1 to n) yield "V" + i.toString,
     arrows = List()).assertFree
     
  def IndiscreteCategoryOn(n:Int) = {
      val forward = for (i <- 1 to n - 1) yield {
      ("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString)}
      val backward = for (i <- 1 to n - 1) yield {
      ("V" + (i+1).toString) --- ("E" + (i+1).toString + i.toString) --> ("V" + i.toString)}
      val isoL = for (i <- 1 to n - 1) yield {
      (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString) --- ("E" + (i+1).toString + i.toString) --> ("V" + i.toString)) ===
        ("V"+ i.toString).identity}
       val isoR = for (i <- 1 to n - 1) yield {
      (("V" + (i+1).toString) --- ("E" + (i+1).toString + i.toString) --> ("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString)) ===
        ("V"+ (i+1).toString).identity}
      Ontology(
          objects = for (i <- 1 to n) yield "V" + i.toString,
          arrows = List.concat(forward, backward),
          relations = List.concat(isoL,isoR)
    )}.assertAcyclic
    

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

  val ReverseGraph = Translation(
    source = Examples.Grph,
    target = Examples.Grph,
    onObjects = Map(
      "an edge" -> "an edge",
      "a vertex" -> "a vertex"),
    onMorphisms = Map(
      ("an edge" --- "has as source" --> "a vertex") -> ("an edge" --- "has as target" --> "a vertex"),
      ("an edge" --- "has as target" --> "a vertex") -> ("an edge" --- "has as source" --> "a vertex")))

  val InitialGraph = Dataset(source = Grph,
    onObjects = Map(
      "an edge" -> List(),
      "a vertex" -> List()),
    onMorphisms = Map(
      ("an edge" --- "has as source" --> "a vertex") -> Map(),
      ("an edge" --- "has as target" --> "a vertex") -> Map()))

  def Chain(n: Int) = Ontology(
    objects = for (i <- 0 to n) yield "V" + i.toString,
    arrows = for (i <- 0 to n - 1) yield {
      ("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString)
    }).assertAcyclic

  val Domain = Translation(
    source = Examples.Chain(0),
    target = Examples.Chain(1),
    onObjects = Map("V0" -> "V0"),
    onMorphisms = Map())

  val Codomain = Translation(
    source = Examples.Chain(0),
    target = Examples.Chain(1),
    onObjects = Map("V0" -> "V1"),
    onMorphisms = Map())

  def Skip(n: Int, k: Int) = {
    //require (n>=k) //TODO explain why this require isn't working.

    val FirstOnMorphisms = (for (i <- 0 to k - 1) yield {
      (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString)) ->
        (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
    }).toMap
    val SecondOnMorphisms = Map(("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString) ->
      (("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString) --- ("E" + (k + 1).toString + (k + 2).toString) --> ("V" + (k + 2).toString)))
    val ThirdOnMorphisms = (for (i <- k + 1 to n - 1) yield {
      (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString)) ->
        (("V" + (i + 1).toString) --- ("E" + (i + 1).toString + (i + 2).toString) --> ("V" + (i + 2).toString))
    }).toMap

    Translation( // [n] --> [n+1] by skipping object k.
      source = Examples.Chain(n),
      target = Examples.Chain(n + 1),
      onObjects =
        (for (i <- 0 to k - 1) yield ("V" + i.toString) -> ("V" + i.toString)).toMap ++
          (for (i <- k to n) yield ("V" + i.toString) -> ("V" + (i + 1).toString)).toMap,
      onMorphisms =
        FirstOnMorphisms ++ SecondOnMorphisms ++ ThirdOnMorphisms)
  }

  def Coface(n: Int, k: Int) = Skip(n, k)

  def Duplicate(n: Int, k: Int) = {
    //require ((n>0) and (n>=k)) //TODO explain why this require isn't working.
    val FirstOnMorphisms = (for (i <- 0 to k - 1) yield {
      (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString)) ->
        (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString))
    }).toMap
    val SecondOnMorphisms = Map(("V" + k.toString) --- ("E" + k.toString + (k + 1).toString) --> ("V" + (k + 1).toString) ->
      ("V" + k.toString).identity)
    val ThirdOnMorphisms = (for (i <- k + 1 to n - 1) yield {
      (("V" + i.toString) --- ("E" + i.toString + (i + 1).toString) --> ("V" + (i + 1).toString)) ->
        (("V" + (i - 1).toString) --- ("E" + (i - 1).toString + i.toString) --> ("V" + i.toString))
    }).toMap

    Translation( //[n]-->[n-1] by duplicating object k. 
      source = Examples.Chain(n),
      target = Examples.Chain(n - 1),
      onObjects =
        (for (i <- 0 to k) yield ("V" + i.toString) -> ("V" + i.toString)).toMap ++
          (for (i <- k + 1 to n) yield ("V" + i.toString) -> ("V" + (i - 1).toString)).toMap,
      onMorphisms = FirstOnMorphisms ++ SecondOnMorphisms ++ ThirdOnMorphisms)
  }

  def Codegeneracy(n: Int, k: Int) = Duplicate(n, k)

  def FiniteCyclicMonoid(n: Int, k: Int) = { //should have k < n. When k = 0, this is the cyclic group of order n.
    require(k < n)
    def composition(i: Int) = (1 to i).foldLeft("an element".identity)({ case (x, m) => x --- "has as successor" --> "an element" })
    Ontology(
      objects = List("an element"),
      arrows = List("an element" --- "has as successor" --> "an element"),
      relations = List(composition(n) === composition(k))).assertFinite
  }

  def TerminalCategoryToFiniteCyclicMonoid(n: Int, k: Int) = Translation( //TODO Need Scott's help.
    source = TerminalCategory,
    target = FiniteCyclicMonoid(n, k),
    onObjects = Map("V0" -> "an element"),
    onMorphisms = Map())

  def TranslationFiniteCyclicMonoids(n1: Int, k1: Int, n2: Int, k2: Int, image: Int) = { //A morphism of finite cyclic monoids is determined by the image of the unique generator. 
    def composition(i: Int) = (1 to i).foldLeft("an element".identity)({ case (x, m) => x --- "has as successor" --> "an element" })

    Translation(
      source = FiniteCyclicMonoid(n1, k1),
      target = FiniteCyclicMonoid(n2, k2),
      onObjects = Map("an element" -> "an element"),
      onMorphisms = Map("an element" --- "has as successor" --> "an element" -> composition(image)))
  }

  val Compose = Translation(
    source = Examples.Chain(1),
    target = Examples.Chain(2),
    onObjects = Map(
      "V0" -> "V0",
      "V1" -> "V2"),
    onMorphisms = Map(
      ("V0" --- "E01" --> "V1") -> ("V0" --- "E01" --> "V1" --- "E12" --> "V2")))

  val TerminalCategory = Examples.Chain(0)

  // Scott says: I fixed this up, but we need to talk about this in some detail!
  // David says: Yes, we should. I don't understand what's going on with Path(_, List(a)).

  def TerminalFunctor(c: Ontology) = Translation(
    source = c,
    target = TerminalCategory,
    onObjects = (for (b <- c.objects) yield (b.name -> "V0")).toMap,
    onMorphisms = (for (a <- c.allGenerators) yield (a.source.name --- a.name --> a.target.name) -> stringAsPath("V0")).toMap //
    )

  def TerminalDataset(c: Ontology) = Dataset(
    source = c,
    onObjects = (for (b <- c.objects) yield (b.name -> List(b.name))).toMap,
    onMorphisms = (for (a <- c.allGenerators) yield (a.source.name --- a.name --> a.target.name) ->
      Map(a.source.name -> a.target.name)).toMap)

  val InitialCategory = Ontology(
    objects = List(),
    arrows = List())

  def InitialFunctor(c: Ontology with Ontologies.Finite) = Translation(
    source = InitialCategory,
    target = c,
    onObjects = Map(),
    onMorphisms = Map())

  def InitialDataset(c: Ontology) = Dataset(
    source = c,
    onObjects = (for (b <- c.objects) yield b.name -> List()).toMap,
    onMorphisms = (for (a <- c.allGenerators) yield (a.source.name --- a.name --> a.target.name) ->
      Map[String, String]()).toMap)

  val SourceFunction = Translation(
    source = Chain(1),
    target = Grph,
    onObjects = Map(
      "V0" -> "an edge",
      "V1" -> "a vertex"),
    onMorphisms = Map(
      ("V0" --- "E01" --> "V1") -> ("an edge" --- "has as source" --> "a vertex")))

  val TargetFunction = Translation(
    source = Chain(1),
    target = Grph,
    onObjects = Map(
      "V0" -> "an edge",
      "V1" -> "a vertex"),
    onMorphisms = Map(
      ("V0" --- "E01" --> "V1") -> ("an edge" --- "has as target" --> "a vertex")))

  val DiscreteDynamicalSystem = Ontology(
    objects = List("an element"),
    arrows = List("an element" --- "has as successor" --> "an element"))

  //  val DDSTimeLapse (n : Int) = Translation (
  //      source = DiscreteDynamicalSystem,
  //      target = DiscreteDynamicalSystem,
  //      onObjects = Map ("an element" -> "an element"),
  //      onMorphisms = Map (
  //          ("an element" --- "has as successor" --> "an element") -> 
  //        	((for (i <- 1 to n) yield {"an element" --- "has as successor"}++)++"an element")))

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

  val PointedSets = Ontology(
    objects = List("an element", "a pointed set"),
    arrows = List(
      "an element" --- "is in" --> "a pointed set",
      "a pointed set" --- "has as chosen" --> "an element"),
    relations = List(
      ("a pointed set" --- "has as chosen" --> "an element" --- "is in" --> "a pointed set")
        ===
        ("a pointed set"))).assertFinite

  val Retraction = PointedSets

  val E2 = Ontology(
    objects = List("0", "1"),
    arrows = List(
      "0" --- "E01" --> "1",
      "1" --- "E10" --> "0")).assertFree

  val E2ToPointedSets = Translation(
    source = E2,
    target = PointedSets,
    onObjects = Map(
      "0" -> "an element",
      "1" -> "a pointed set"),
    onMorphisms = Map(
      ("0" --- "E01" --> "1") -> ("an element" --- "is in" --> "a pointed set"),
      ("1" --- "E10" --> "0") -> ("a pointed set" --- "has as chosen" --> "an element")))

  val PointedSetsToIsomorphism = Translation(
    source = PointedSets,
    target = Isomorphism,
    onObjects = Map(
      "an element" -> "0",
      "a pointed set" -> "1"),
    onMorphisms = Map(
      ("an element" --- "is in" --> "a pointed set") -> ("0" --- "E01" --> "1"),
      ("a pointed set" --- "has as chosen" --> "an element") -> ("1" --- "E10" --> "0")))

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