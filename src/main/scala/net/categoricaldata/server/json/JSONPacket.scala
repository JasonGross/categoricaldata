package net.categoricaldata.server.json

trait JSONPacket { packet =>
	def json: Option[String]
	def provenance: Option[Provenance]
	def updateProvenance(provenance: Provenance): JSONPacket
	def updateProvenanceIfEmpty(provenance: Provenance) = {
	  if(packet.provenance.isEmpty) {
	    updateProvenance(provenance)
	  } else {
	    this
	  }
	}
	def fillEmptyJSON: JSONPacket
	def getJSON = fillEmptyJSON.json.get
}
object JSONPacket {
  implicit def ontology2JSONOntology(ontology: net.categoricaldata.ontology.Ontology) = Pack.packOntology(ontology)
  implicit def translation2JSONTranslation(translation: net.categoricaldata.ontology.Translation) = Pack.packTranslation(translation)
  implicit def dataset2JSONOntology(dataset: net.categoricaldata.ontology.Dataset) = Pack.packDataset(dataset)
}