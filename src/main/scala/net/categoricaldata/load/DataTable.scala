package net.categoricaldata.load
import net.categoricaldata.ontology._

case class DataTables(tables: List[DataTable]) {
  val source: Ontology = {
    Ontology(objects = tables.map(_.primaryKey), arrows = ???, relations = Nil)
  }
  def toPartialDataset: Translation = new Translation {
    override val source = {
      Ontology(objects = ???, arrows = ???, relations = ???)
    }
    override val target = source
    override def onObjects(o: Box) = ???
    override def onGenerators(g: Arrow) = ???
  }
  def toDataset: source.Dataset = ???
}

case class DataTable(primaryKey: String, foreignKeys: List[String], rows: Map[String, List[String]])