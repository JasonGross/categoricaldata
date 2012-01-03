package net.metaphor.server.transformers
import com.recursivity.commons.bean.StringValueTransformer



class OntologyTransformer extends StringValueTransformer[net.metaphor.api.Ontology] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def toValue(from: String): Option[net.metaphor.api.Ontology] = Some(parse(from).extract[net.metaphor.json.Ontology]).map(_.unpack)
}
class FiniteTranslationTransformer extends StringValueTransformer[net.metaphor.api.FiniteTranslation] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def toValue(from: String): Option[net.metaphor.api.FiniteTranslation] = Some(parse(from).extract[net.metaphor.json.Translation]).map(_.unpack)
}
class DatasetTransformer extends StringValueTransformer[net.metaphor.api.Dataset] {
  implicit val formats = net.liftweb.json.DefaultFormats
  import net.liftweb.json.JsonParser.parse

  def toValue(from: String): Option[net.metaphor.api.Dataset] = Some(parse(from).extract[net.metaphor.json.Dataset]).map(_.unpack)
}