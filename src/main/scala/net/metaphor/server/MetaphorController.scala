package net.metaphor.server

import org.bowlerframework.controller.Controller
import org.bowlerframework.controller.FunctionNameConventionRoutes
import org.bowlerframework.Request
import net.metaphor.examples.Examples

class MetaphorController extends Controller with FunctionNameConventionRoutes {

  def `GET /metaphor/examples/Ord/:n`(n: Int) = Examples.Ord(n).toJSON
  def `GET /metaphor/examples/TerminalBigraph` = Examples.TerminalBigraph.toJSON

}