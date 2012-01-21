package net.categoricaldata.load
import net.categoricaldata.ontology._

case class DataTables(tables: List[DataTable]) {
  val source: Ontology = {
    Ontology(
        objects = tables.map(_.primaryKey), 
        arrows = for(t <- tables; p = t.primaryKey; f <- t.foreignKeys) yield {
          import net.categoricaldata.dsl.Sentences._
          p --- ": " --> f 
        },
        relations = Nil)
  }
  def toPartialDataset: source.PartialDataset = new source.PartialDataset {
    override val source = {
      Ontology(
          objects = ???, 
          arrows = ???, 
          relations = ???)
    }
    override def onObjects(o: Box) = ???
    override def onGenerators(g: Arrow) = ???
  }
  def toDataset: source.Dataset = toPartialDataset.toDataset
}

case class DataTable(primaryKey: String, foreignKeys: List[String], rows: Map[String, List[String]])