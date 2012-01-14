package net.metaphor.server

import org.bowlerframework.controller.Controller
import org.bowlerframework.controller.FunctionNameConventionRoutes
import org.bowlerframework.Request
import net.metaphor.examples.Examples
import net.metaphor.api.Ontology
import net.metaphor.api.Dataset
import net.metaphor.api.Translation

class MetaphorController extends Controller with FunctionNameConventionRoutes {

  // TODO remove the need to write toJSON everywhere
  // TODO use reflection to look up methods on Examples (also, to generate lists)
  
  // TODO toJSON should embed the request URL
  // TODO when extracting from JSON, should store the original representation
  
  def `GET /metaphor/examples/ontologies/Chain/:n`(n: Int): Ontology = Examples.Chain(n)
  def `GET /metaphor/examples/ontologies/Isomorphism`: Ontology = Examples.Isomorphism
  def `GET /metaphor/examples/ontologies/Grph`: Ontology = Examples.Grph
  def `GET /metaphor/examples/datasets/TerminalBigraph`: Dataset = Examples.TerminalBigraph
  def `GET /metaphor/examples/translations/ReverseGraph`: Translation = Examples.ReverseGraph

  def `GET /metaphor/compute/leftPushforward`(translation: Translation, dataset: Dataset): Dataset = {
    translation.__!(dataset)
  }
}