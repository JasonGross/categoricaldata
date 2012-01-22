package net.categoricaldata.server.transformers
import com.recursivity.commons.bean.StringValueTransformer
import scala.io.Source
import org.apache.http.util.EntityUtils
import org.apache.http.impl.client.ContentEncodingHttpClient
import org.apache.http.client.methods.HttpGet
import org.bowlerframework.RequestScope
import java.net.URL
import org.bowlerframework.http.BowlerHttpRequest

trait StringOrURLTransformer[A] extends StringValueTransformer[A] {
  def stringToValue(from: String): Option[A]

  private def slurp(url: String): (Int, String) = {
    val get = new HttpGet(url)
    get.setHeader("Accept", "application/json")
    val response = new ContentEncodingHttpClient().execute(get)
    val entity = response.getEntity();
    val status = response.getStatusLine().getStatusCode()
    (status, EntityUtils.toString(entity))
  }

  private def serverURL = new URL(RequestScope.request.asInstanceOf[BowlerHttpRequest].getHttpServletRequest.getRequestURL.toString) match {
      case url => url.getProtocol() + "://" + url.getHost() + ((url.getProtocol(), url.getPort()) match {
        case ("http", p) if p == 80 => ""
        case ("https", p) if p == 443 => ""
        case (_, p) => ":" + p.toString
      })
    }
  
  def toValue(from: String) = {
    stringToValue(from match {
      case from if from.startsWith("http") => slurp(from)._2
      case from if from.startsWith("/") => slurp(serverURL + from)._2
      case _ => from
    })
  }
}

class OntologyTransformer extends StringOrURLTransformer[net.categoricaldata.ontology.Ontology] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def stringToValue(from: String): Option[net.categoricaldata.ontology.Ontology] = Some(parse(from).extract[net.categoricaldata.server.json.Ontology].copy(json = Some(from))).map(_.unpack)
}
class TranslationTransformer extends StringOrURLTransformer[net.categoricaldata.ontology.Translation] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def stringToValue(from: String): Option[net.categoricaldata.ontology.Translation] = Some(parse(from).extract[net.categoricaldata.server.json.Translation].copy(json = Some(from))).map(_.unpack)
}
class DatasetTransformer extends StringOrURLTransformer[net.categoricaldata.ontology.Dataset] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def stringToValue(from: String): Option[net.categoricaldata.ontology.Dataset] = Some(parse(from).extract[net.categoricaldata.server.json.Dataset].copy(json = Some(from))).map(_.unpack)
}