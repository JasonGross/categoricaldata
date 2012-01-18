package net.categoricaldata.util
import net.categoricaldata.ontology.Ontology
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import net.categoricaldata.category.NaturalTransformationToSet

trait CustomMatchers {
  class DatasetIsomorphismMatcher(right: Ontology#Dataset) extends Matcher[Ontology#Dataset] {
    def apply(left: Ontology#Dataset) = {
      MatchResult(
        left.isIsomorphicTo(right),
        "The data sets are not isomorphic",
        "The data sets are isomorphic")
    }
  }
  class OntologyIsomorphismMatcher(right: Ontology) extends Matcher[Ontology] {
    def apply(left: Ontology) = {
      MatchResult(
        left.isIsomorphicTo(right),
        "The ontologes are not isomorphic",
        "The ontologies are isomorphic")
    }
  }
  class OntologyEquivalenceMatcher(right: Ontology) extends Matcher[Ontology] {
    def apply(left: Ontology) = {
      MatchResult(
        left.isEquivalentTo(right),
        "The ontologes are not equivalent",
        "The ontologies are equivalent")
    }
  }
  
  def beIsomorphicTo(d: Ontology#Dataset) = new DatasetIsomorphismMatcher(d)
  def beIsomorphicTo(o: Ontology) = new OntologyIsomorphismMatcher(o)
  def beEquivalentTo(o: Ontology) = new OntologyEquivalenceMatcher(o)  
}