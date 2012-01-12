package net.metaphor.api
import net.metaphor.examples.Examples
import net.tqft.toolkit.collections.NonStrictNaturalNumbers
import net.tqft.toolkit.Profiler

object RightPushforwardProfiler1 extends App with Profiler {
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

        
  for(t <- movingTimingAverages(10) {
    val LHS = Examples.ReverseGraph.__*(DavidsFunkyGraph)
    val RHS = DavidsFunkyGraphReversed
    LHS.isIsomorphicTo(RHS)
  }) println(t)

}