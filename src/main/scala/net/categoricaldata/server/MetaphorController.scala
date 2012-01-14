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
  // TODO when extracting from JSON, should store the original representation

  def `GET /metaphor/examples/ontologies/Chain/:n`(n: Int): Ontology = Examples.Chain(n)
  def `GET /metaphor/examples/ontologies/Isomorphism`: Ontology = Examples.Isomorphism
  def `GET /metaphor/examples/ontologies/Grph`: Ontology = Examples.Grph
  def `GET /metaphor/examples/ontologies/FiniteCyclicMonoid/:n/:k` (n: Int, k:Int): Ontology = Examples.FiniteCyclicMonoid (n,k)
  def `GET /metaphor/examples/datasets/TerminalBigraph`: Dataset = Examples.TerminalBigraph
  def `GET /metaphor/examples/translations/ReverseGraph`: Translation = Examples.ReverseGraph

  def `GET /metaphor/compute/leftPushforward`(translation: Translation, dataset: Dataset): Dataset = {
    translation.__!(dataset)
  }
}