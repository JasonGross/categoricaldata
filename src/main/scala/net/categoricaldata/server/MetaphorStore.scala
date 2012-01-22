package net.categoricaldata.server
import net.categoricaldata.server.json.JSONPacket
import net.categoricaldata.util.SHA1
import net.tqft.toolkit.amazon.S3
import net.categoricaldata.ontology._
import net.categoricaldata.server.transformers._

object MetaphorStore {

  val bucket = S3("metaphor-store")

  def add(packet: JSONPacket): String = {
    val json = packet.getJSON
    val hash = SHA1(json)
    bucket += (hash -> json)
    hash
  }
  
  def lookupOntology(hash: String): Ontology = {
    (new OntologyTransformer).stringToValue(bucket(hash)).get
  }
  def lookupTranslation(hash: String): Translation = {
    (new TranslationTransformer).stringToValue(bucket(hash)).get
  }
  def lookupDataset(hash: String): Dataset = {
    (new DatasetTransformer).stringToValue(bucket(hash)).get
  }
}