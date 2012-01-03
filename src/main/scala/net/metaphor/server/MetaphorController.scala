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
 
  def `GET /metaphor/compute/leftPushforward`(translation: String, dataset: String): Dataset = {
    implicit val formats = net.liftweb.json.DefaultFormats
    import net.liftweb.json.JsonParser.parse
    val translationJSON = {
      parse(translation).extract[Translation]
    }
    val datasetJSON = {
      parse(dataset).extract[Dataset]
    }
    val F = translationJSON.unpack
    val i = F.source.internalize(datasetJSON.unpack)
    F.leftPushforward(i).toJSON
  }
}