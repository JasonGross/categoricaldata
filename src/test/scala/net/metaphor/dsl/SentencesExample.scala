package net.metaphor.dsl

object SentencesExample extends App {
  import Sentences._

  // TODO, allow descriptions  
  // "a fusion object" is "a simple object in a unitary tensor category with duals",
  val C = model(
    objects = List("a fusion object", "a graph", "a fusion category", "a positive ring"),
    arrows = List("a fusion object" --- "has as principal graph" --> "a graph",
      "a fusion object" --- "sits inside" --> "a fusion category",
      "a fusion category" --- "has as fusion ring" --> "a positive ring") //
      )

  // FIXME
  val D = C

  // TODO allow relations
  /*
   *  possible examples:
   *  f;g == h
   *  "F" --- "h" --> "G" --- "g" --> "H" equals "F" --- "k" --> "H"
   */

  // Functors
  /*
   * On objects:
   * 	"F" mapsto "F2"
   * (Maybe you can leave these out, and they'll be inferred?)
   * 
   * On morphisms:
   * Specify them explicitly (in case there are arrows with duplicate names):
   * 	"F" --- "h" --> "G" mapsto "F2" --- "h2" --> "G2"
   * TODO This should 'guess' sources and targets:
   * 	"h" mapsto "h2"
   * 
   * Note: you can write 'maps to' instead of 'mapsto', but then you need parentheses around the right hand side.
   * 
   * Maybe for later: proof obligations for relations.
   */

  val F = functor(
    source = C,
    target = D,
    onObjects = Map("F" -> "F2"),
    onMorphisms = Map(("F" --- "h" --> "G") -> ("F2" --- "h2" --> "G2")) //
    )

  val i = dataset(source = C,
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