package net.categoricaldata.server
import net.liftweb.json.Formats
import org.bowlerframework.view.ViewRenderer
import org.bowlerframework.Request
import org.bowlerframework.Response
import org.bowlerframework.exception.HttpException
import java.net.URL
import org.bowlerframework.http.BowlerHttpRequest
import org.bowlerframework.view.scalate.ScalateViewRenderer
import org.bowlerframework.view.ViewPath

class ModelViewRenderer(jsonFormats: Formats = net.liftweb.json.DefaultFormats) extends ViewRenderer {
  val prettyJsonViewRenderer = new PrettyJsonViewRenderer(jsonFormats)

  def onError(request: Request, response: Response, exception: Exception) = prettyJsonViewRenderer.onError(request, response, exception)

  def renderView(request: Request, response: Response, models: Seq[Any]) = {
    import net.categoricaldata.server.json._

    def completeURL = {
      val servletRequest = request.asInstanceOf[BowlerHttpRequest].getHttpServletRequest
      val queryString = servletRequest.getQueryString() match {
        case null => ""
        case "" => ""
        case q => "?" + q
      }
      servletRequest.getRequestURL.toString + queryString
    }
    lazy val serverURL = new URL(request.asInstanceOf[BowlerHttpRequest].getHttpServletRequest.getRequestURL.toString) match {
      case url => url.getProtocol() + "://" + url.getHost() + ((url.getProtocol(), url.getPort()) match {
        case ("http", p) if p == 80 => ""
        case ("https", p) if p == 443 => ""
        case (_, -1) => ""
        case (_, p) => ":" + p.toString
      })
    }

    val processedModels = models map {
      case hints: apiHints => apiHints(hints.hints.map(hint => hint.copy(path = serverURL + hint.path)))
      case ontology: net.categoricaldata.ontology.Ontology => Pack.packOntology(ontology)
      case dataset: net.categoricaldata.ontology.Ontology#Dataset => Pack.packDataset(dataset)
      case translation: net.categoricaldata.ontology.Translation => Pack.packTranslation(translation)
      case other => other
    } map {
      case packet: JSONPacket => packet.updateProvenanceIfEmpty(Provenance(url = Some(completeURL)))
      case other => other
    }

    val renderJSON = request.getHeader("accept") match {
      case None => false
      case Some(accept) => {
        val lower = accept.toLowerCase
        lower.contains("text/javascript") || lower.contains("application/javascript") || lower.contains("text/json") || lower.contains("application/json")
      }
    }

    val locationString = request.getPath + (request.getParameterMap match {
      case m if m.isEmpty => ""
      case m => "?" + m.collect({case (name, value: String) => name + "=" + value}).mkString("&")
    })
    response.addHeader("Location", locationString)
    
    if (renderJSON) {
      prettyJsonViewRenderer.renderView(request, response, processedModels)
    } else {
      val filledModels = processedModels map {
        case packet: JSONPacket => packet.fillEmptyJSON
        case other => other
      }

      val viewPath: ViewPath = filledModels.head match {
        case _: Ontology => "/metaphor/show/ontology"
        case _: Dataset => "/metaphor/show/dataset"
        case _: Translation => "/metaphor/show/translation"
        case _: apiHints => "/metaphor/show/apihints"
      }
      request.setMappedPath(viewPath.path)
      request.setMethod(viewPath.method)

      new ScalateViewRenderer().renderView(request, response, filledModels)
    }

  }
}