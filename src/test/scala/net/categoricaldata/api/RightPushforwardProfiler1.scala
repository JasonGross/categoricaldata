package net.categoricaldata.category
import net.categoricaldata.examples.Examples
import net.tqft.toolkit.collections.NonStrictNaturalNumbers
import net.tqft.toolkit.Profiler

object RightPushforwardProfiler1 extends App with Profiler {
  import net.categoricaldata.dsl.Sentences._

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

        // 2011-01-13	807ms
        //				546ms improving FunctorToSet.Section.equals
        //				468ms SliceCategory now comes 'with CachingGenerators'
        //				442ms removing redundant Set.toList
        
  for (	
    t <- movingTimingAverages(10) {
      for (n <- 0 until 1000) {
        val LHS = Examples.ReverseGraph.__*(DavidsFunkyGraph)
        val RHS = DavidsFunkyGraphReversed
        LHS.isIsomorphicTo(RHS)
      }
    }
  ) println(t)

}