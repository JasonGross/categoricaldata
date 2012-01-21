package net.categoricaldata.server

import org.bowlerframework.controller.Controller
import org.bowlerframework.controller.FunctionNameConventionRoutes
import org.bowlerframework.Request
import net.categoricaldata.examples.Examples
import net.categoricaldata.ontology.Ontology
import net.categoricaldata.ontology.Dataset
import net.categoricaldata.ontology.Translation
import net.categoricaldata.ontology.Box

class MetaphorController extends Controller with FunctionNameConventionRoutes {

  // TODO use reflection to look up methods on Examples (also, to generate lists)

  // TODO toJSON should embed the request URL
  // TODO when extracting from JSON, should store the original representation (in progress)

  // each entry contains the name of an Ontology, and a list of parameter names and example values.
  val exampleOntologies: List[(String, List[(String, String)])] = List(
      ("Chain", List(("n", "4"))),
      ("Isomorphism", Nil),
      ("Graph", Nil),
      ("FiniteCyclicMonoid", List(("n", "5"), ("m", "3"))))
  def `GET /metaphor/examples` = ??? // work out how to render exampleOntologies as some HTML
  
  def `GET /metaphor/examples/ontologies/Chain`(n: Int): Ontology = Examples.Chain(n)
  def `GET /metaphor/examples/ontologies/Isomorphism`: Ontology = Examples.Isomorphism
  def `GET /metaphor/examples/ontologies/Graph`: Ontology = Examples.Graph
  def `GET /metaphor/examples/ontologies/FiniteCyclicMonoid` (n: Int, k:Int): Ontology = Examples.FiniteCyclicMonoid (n,k)
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
  
  def `GET /metaphor/compute/grothendieck`(dataset: Dataset): Ontology = {
    dataset.grothendieck
  }
  def `GET /metaphor/compute/yoneda`(ontology: Ontology, box: String): Dataset = {
    ontology.yoneda(Box(box))
  }
  def `GET /metaphor/compute/translationToDataset`(translation: Translation): Dataset = {
    translation.asPartialDataset.toDataset
  }
}