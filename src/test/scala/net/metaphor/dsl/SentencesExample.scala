package net.metaphor.dsl

object SentencesExample extends App {
  import Sentences._

  // TODO, allow descriptions  
  // "a fusion object" is "a simple object in a unitary tensor category with duals",
  val C = Ontology(
    objects = List("a fusion object", "a graph", "a fusion category", "a positive ring"),
    arrows = List("a fusion object" --- "has as principal graph" --> "a graph",
      "a fusion object" --- "sits inside" --> "a fusion category",
      "a fusion category" --- "has as fusion ring" --> "a positive ring") //
      )

  // FIXME
  val D = C

  // TODO allow relations
  /*
   *  possible example:
   *  "F" --- "h" --> "G" --- "g" --> "H" equals "F" --- "k" --> "H"
   */

  // Functors
  /* 
   * Maybe for later: proof obligations for relations.
   */

  val F = Translation(
    source = C,
    target = D,
    onObjects = Map("F" -> "F2"),
    onMorphisms = Map(("F" --- "h" --> "G") -> ("F2" --- "h2" --> "G2")) //
    )

  val i = Dataset(source = C,
    onObjects = Map(
      "a fusion object" -> List("X", "Y"),
      "a graph" -> List("G1", "G2") //
      ),
    onMorphisms = Map(
      ("a fusion object" --- "has as principal graph" --> "a graph") -> Map(
        "X" -> "G1",
        "Y" -> "G1") //
        ) //
        )
        
}