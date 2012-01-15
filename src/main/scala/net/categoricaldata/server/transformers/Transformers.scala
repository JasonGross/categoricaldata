package net.categoricaldata.server.transformers
import com.recursivity.commons.bean.StringValueTransformer



class OntologyTransformer extends StringValueTransformer[net.categoricaldata.api.Ontology] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def toValue(from: String): Option[net.categoricaldata.api.Ontology] = Some(parse(from).extract[net.categoricaldata.json.Ontology].copy(json = Some(from))).map(_.unpack)
}
class TranslationTransformer extends StringValueTransformer[net.categoricaldata.api.Translation] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def toValue(from: String): Option[net.categoricaldata.api.Translation] = Some(parse(from).extract[net.categoricaldata.json.Translation]).map(_.unpack)
}
class DatasetTransformer extends StringValueTransformer[net.categoricaldata.api.Dataset] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def toValue(from: String): Option[net.categoricaldata.api.Dataset] = Some(parse(from).extract[net.categoricaldata.json.Dataset]).map(_.unpack)
}