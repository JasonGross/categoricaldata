package net.categoricaldata.server.transformers
import com.recursivity.commons.bean.StringValueTransformer



class OntologyTransformer extends StringValueTransformer[net.categoricaldata.ontology.Ontology] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def toValue(from: String): Option[net.categoricaldata.ontology.Ontology] = Some(parse(from).extract[net.categoricaldata.server.json.Ontology].copy(json = Some(from))).map(_.unpack)
}
class TranslationTransformer extends StringValueTransformer[net.categoricaldata.ontology.Translation] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def toValue(from: String): Option[net.categoricaldata.ontology.Translation] = Some(parse(from).extract[net.categoricaldata.server.json.Translation].copy(json = Some(from))).map(_.unpack)
}
class DatasetTransformer extends StringValueTransformer[net.categoricaldata.ontology.Dataset] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def toValue(from: String): Option[net.categoricaldata.ontology.Dataset] = Some(parse(from).extract[net.categoricaldata.server.json.Dataset].copy(json = Some(from))).map(_.unpack)
}