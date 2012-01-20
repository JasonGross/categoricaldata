package net.categoricaldata.server.transformers
import com.recursivity.commons.bean.StringValueTransformer
import scala.io.Source

trait StringOrURLTransformer[A] extends StringValueTransformer[A] {
  def stringToValue(from: String): Option[A]
 
  def toValue(from: String) = {
    stringToValue(if (from.startsWith("http")) {
      Source.fromURL(from).mkString
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