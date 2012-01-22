package net.categoricaldata.server.transformers
import com.recursivity.commons.bean.StringValueTransformer
import scala.io.Source
import org.apache.http.util.EntityUtils
import org.apache.http.impl.client.ContentEncodingHttpClient
import org.apache.http.client.methods.HttpGet

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

  def toValue(from: String) = {
    stringToValue(if (from.startsWith("http")) {
      slurp(from)._2
    } else {
      from
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