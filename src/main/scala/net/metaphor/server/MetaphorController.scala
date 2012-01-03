package net.metaphor.server

import org.bowlerframework.controller.Controller
import org.bowlerframework.controller.FunctionNameConventionRoutes
import org.bowlerframework.Request
import net.metaphor.examples.Examples
import net.metaphor.api.FiniteTranslation
import net.metaphor.api.Ontology
import net.metaphor.api.Dataset

class MetaphorController extends Controller with FunctionNameConventionRoutes {

  def `GET /metaphor/examples/ontologies/Ord/:n`(n: Int) = Examples.Ord(n).toJSON
  def `GET /metaphor/examples/ontologies/Isomorphism` = Examples.Isomorphism.toJSON
  def `GET /metaphor/examples/ontologies/Grph` = Examples.Grph.toJSON
  def `GET /metaphor/examples/datasets/TerminalBigraph` = Examples.TerminalBigraph.toJSON
  def `GET /metaphor/examples/translations/ReverseGraph` = Examples.ReverseGraph.toJSON

  def `GET /metaphor/compute/leftPushforward`(translation: FiniteTranslation, dataset: Dataset): net.metaphor.json.Dataset = {
    val i = translation.source.internalize(dataset)
    translation.leftPushforward(i).toJSON
  }
}