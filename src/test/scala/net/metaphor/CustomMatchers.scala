package net.metaphor
import net.metaphor.api.Ontology
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

trait CustomMatchers {
  class IsomorphismMatcher(right: Ontology#Dataset) extends Matcher[Ontology#Dataset] {
    def apply(left: Ontology#Dataset) = {
      MatchResult(
        left.isIsomorphicTo(right),
        "The data sets are not isomorphic",
        "The data sets are isomorphic")
    }
  }

  def beIsomorphicTo(d: Ontology#Dataset) = new IsomorphismMatcher(d)
}