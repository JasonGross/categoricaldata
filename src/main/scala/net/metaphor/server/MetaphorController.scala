package net.metaphor.server

import org.bowlerframework.controller.Controller
import org.bowlerframework.controller.FunctionNameConventionRoutes
import org.bowlerframework.Request
import net.metaphor.examples.Examples
import net.metaphor.json._

class MetaphorController extends Controller with FunctionNameConventionRoutes {

  def `GET /metaphor/examples/ontologies/Ord/:n`(n: Int) = Examples.Ord(n).toJSON
  def `GET /metaphor/examples/ontologies/Isomorphism` = Examples.Isomorphism.toJSON
  def `GET /metaphor/examples/ontologies/Grph` = Examples.Grph.toJSON
  def `GET /metaphor/examples/datasets/TerminalBigraph` = Examples.TerminalBigraph.toJSON
  def `GET /metaphor/examples/translations/ReverseGraph` = Examples.ReverseGraph.toJSON
  
  def `POST /metaphor/compute/leftPushforward`(translation: Translation, dataset: Dataset): Dataset = {
    val F = translation.unpack
    val i = F.source.internalize(dataset.unpack)
    F.leftPushforward(i).toJSON
  } 
}