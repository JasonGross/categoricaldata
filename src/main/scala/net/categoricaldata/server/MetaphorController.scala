package net.categoricaldata.server

import org.bowlerframework.controller.Controller
import org.bowlerframework.controller.FunctionNameConventionRoutes
import org.bowlerframework.Request
import net.categoricaldata.examples.Examples
import net.categoricaldata.api.Ontology
import net.categoricaldata.api.Dataset
import net.categoricaldata.api.Translation

class MetaphorController extends Controller with FunctionNameConventionRoutes {

  // TODO use reflection to look up methods on Examples (also, to generate lists)

  // TODO toJSON should embed the request URL
  // TODO when extracting from JSON, should store the original representation (in progress)

  // each entry contains the name of an Ontology, and a list of parameter names and example values.
  val exampleOntologies: List[(String, List[(String, String)])] = List(
      ("Chain", List(("n", "4"))),
      ("Isomorphism", Nil),
      ("Grph", Nil),
      ("FiniteCyclicMonoid", List(("n", "5"), ("m", "3"))))
  def `GET /metaphor/examples` = ??? // work out how to render exampleOntologies as some HTML
  
  def `GET /metaphor/examples/ontologies/Chain/:n`(n: Int): Ontology = Examples.Chain(n)
  def `GET /metaphor/examples/ontologies/Isomorphism`: Ontology = Examples.Isomorphism
  def `GET /metaphor/examples/ontologies/Grph`: Ontology = Examples.Grph
  def `GET /metaphor/examples/ontologies/FiniteCyclicMonoid/:n/:k` (n: Int, k:Int): Ontology = Examples.FiniteCyclicMonoid (n,k)
  def `GET /metaphor/examples/datasets/TerminalBigraph`: Dataset = Examples.TerminalBigraph
  def `GET /metaphor/examples/translations/ReverseGraph`: Translation = Examples.ReverseGraph

  def `GET /metaphor/compute/leftPushforward`(translation: Translation, dataset: Dataset): Dataset = {
    translation.__!(dataset)
  }
  def `GET /metaphor/compute/rightPushforward`(translation: Translation, dataset: Dataset): Dataset = {
    translation.__*(dataset)
  }
  def `GET /metaphor/compute/pullback`(translation: Translation, dataset: Dataset): Dataset = {
    translation.^*(dataset)
  }
}