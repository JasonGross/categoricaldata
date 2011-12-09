package net.metaphor.dsl

object SentencesExample extends App {
  import Sentences._
  
  val C = model(
    // TODO, allow descriptions  
    // "a fusion object" is "a simple object in a unitary tensor category with duals",
    "a fusion object" --- "has as principal graph" --> "a graph", 
    "a fusion object"  --- "sits inside" --> "a fusion category",
    "a fusion category" --- "has as fusion ring" --> "a positive ring"
  )
  // TODO allow relations
  /*
   *  possible examples:
   *  f;g == h
   *  "F" --- "h" --> "G" --- "g" --> "H" equals "F" --- "k" --> "H"
   */
  
  // TODO describe functors!
  /*
   * On objects:
   * 	"F" maps to "F2"
   * (Maybe you can leave these out, and they'll be inferred?)
   * 
   * On morphisms:
   * Specify them explicitly (in case there are arrows with duplicate names):
   * 	"F" --- "h" --> "G" maps to "F2" --- "h2" --> "G2"
   * TODO This should 'guess' sources and targets:
   * 	"h" maps to "h2"
   * 
   * val F = functor(C ==> D)(
   * 	"F" maps to "F2"
   * )(
   * 	"F" --- "h" --> "G" maps to "F2" --- "h2" --> "G2"
   * )
   * 
   * Maybe for later: proof obligations for relations.
   */
  
  // TODO describe data!
  /*
   * val i = dataset(C ==> Set)(
   * 	"a fusion object" maps to ("X", "Y"),
   * 	"a graph" maps to ("G1", "G2")
   * )(
   * 	"a fusion object" --- "has as principal graph" --> "a graph" maps to (
   * 		"X" -> "G1",
   *    	"Y" -> "G1"
   *  	)
   *  )
   * 
   * TODO, guess sources and targets?
   */
  
}