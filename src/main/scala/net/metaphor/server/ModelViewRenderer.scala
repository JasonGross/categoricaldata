package net.metaphor.server
import net.liftweb.json.Formats
import org.bowlerframework.view.ViewRenderer
import org.bowlerframework.Request
import org.bowlerframework.Response
import org.bowlerframework.exception.HttpException

class ModelViewRenderer(jsonFormats: Formats = net.liftweb.json.DefaultFormats) extends ViewRenderer {
  val prettyJsonViewRenderer = new PrettyJsonViewRenderer(jsonFormats)

  def onError(request: Request, response: Response, exception: Exception) = prettyJsonViewRenderer.onError(request, response, exception)

  // 'extractParameters' pulls out the request parameters, with values as Strings, and also the page number separately, as an Int
  private def extractParameters(request: Request): Map[String, String] = {
    // take all the 'string' parameters (which should throw out Bowler's extras, such as "splat" and any named parameters)
    // also throw out any "jsonp" parameter, as the prettyJsonViewRenderer deals with that, not the Query
    (request.getParameterMap collect { case (key, value: String) => (key, value) }) - "jsonp"
  }

  def renderView(request: Request, response: Response, models: Seq[Any]) = {
    val parameters = extractParameters(request)
    prettyJsonViewRenderer.renderView(request, response,
      models collect {
        case ontology: net.metaphor.api.Ontology => new net.metaphor.json.Ontology(ontology)
        case x => x
      })
  }
}