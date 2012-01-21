package net.categoricaldata.load
import net.categoricaldata.ontology._

case class DataTables(tables: List[DataTable]) { dataTables =>
  val source: Ontology = {
    Ontology(
      objects = tables.map(_.primaryKey),
      arrows = for (t <- tables; p = t.primaryKey; f <- t.foreignKeys) yield {
        import net.categoricaldata.dsl.Sentences._
        p --- "has foreign key" --> f
      },
      relations = Nil)
  }

  private def tableWithPrimaryKey(key: String): DataTable = tables.find(_.primaryKey == key).get

  def toPartialDataset: source.PartialDataset = new source.PartialDataset { partialDataset =>
    case class PartialDatasetBox(value: String, over: Box) extends Box {
      def name = over.name + ":= " + value
    }

    override val source = {
      new Ontology {
        override def objectsAtLevel(k: Int) = {
          if (k == 0) {
            for (t <- tables; p = t.primaryKey; (r, _) <- t.rows) yield PartialDatasetBox(r, Box(p))
          } else {
            Nil
          }
        }
        override val minimumLevel = 0
        override val maximumLevel = 0
        override def generators(s: O, t: O) = {
          (s, t) match {
            case (PartialDatasetBox(sourceValue, sourceOver), PartialDatasetBox(targetValue, targetOver)) => {
              dataTables.source.generators(sourceOver, targetOver) collect {
                case Arrow(sourceOver, targetOver, "has foreign key") if (tableWithPrimaryKey(sourceOver.name).rows(sourceValue)(targetOver.name) == targetValue) => {
                  Arrow(s, t, "maps to")
                }
              }
            }
          }
        }
        override def relations(s: O, t: O) = ???
        override def pathEquality(path1: Path, path2: Path) = ???
      }
    }
    override def onObjects(o: Box) = o match {
      case PartialDatasetBox(value, over) => over
    }
    override def onGenerators(g: Arrow) = g match {
      case Arrow(PartialDatasetBox(_, sourceOver), PartialDatasetBox(_, targetOver), "maps to") => dataTables.source.generatorAsMorphism(Arrow(sourceOver, targetOver, "has foreign key"))
    }
  }
  def toDataset: source.Dataset = toPartialDataset.toDataset
}

case class DataTable(primaryKey: String, foreignKeys: List[String], rows: Map[String, String => String])