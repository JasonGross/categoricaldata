package net.metaphor.api
import net.metaphor.examples.Examples
import net.tqft.toolkit.collections.NonStrictNaturalNumbers
import net.tqft.toolkit.Profiler

object RightPushforwardProfiler2 extends App with Profiler {
  import net.metaphor.dsl.Sentences._

  // 2012-01-12 41ms
  //            25ms: Path.hashCode as a lazy val.
  
  for (
    t <- movingTimingAverages(10) {
      val FCM4_3 = Dataset(source = Examples.FiniteCyclicMonoid(4, 3),
        onObjects = Map(
          "an element" -> List("a", "b")),
        onMorphisms = Map(
          "an element" --- "has as successor" --> "an element" -> Map(
            "a" -> "b",
            "b" -> "b")))

      val FCM4_3Times2RToFCM5_3 = Dataset(source = Examples.FiniteCyclicMonoid(5, 3),
        onObjects = Map(
          "an element" -> List("aa", "ab", "ba", "bb")),
        onMorphisms = Map(
          "an element" --- "has as successor" --> "an element" -> Map(
            "aa" -> "ba",
            "ab" -> "ba",
            "ba" -> "bb",
            "bb" -> "bb")))

      val X = FCM4_3
      val LHS = Examples.TranslationFiniteCyclicMonoids(4, 3, 5, 3, 2) __* (X)
      val RHS = FCM4_3Times2RToFCM5_3
      LHS.isIsomorphicTo(RHS)
    }
  ) println(t)

}