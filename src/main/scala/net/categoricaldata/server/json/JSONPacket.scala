package net.categoricaldata.server.json

trait JSONPacket {
	def json: Option[String]
	def provenance: Option[Provenance]
	def updateProvenance(provenance: Provenance): JSONPacket
	def fillEmptyJSON: JSONPacket
}