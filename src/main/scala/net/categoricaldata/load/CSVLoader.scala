package net.categoricaldata.load

object CSVLoader extends DataLoader {
	def fromLines(lines: Iterable[String]): DataTables = {
	  import net.tqft.toolkit.collections.Split._
	  
	  val tableLines = lines.splitOn(_.isEmpty).filter(_.nonEmpty)
	  
	  def splitFields(line: String) = line.split(',').toList
	  def parseRow(row: String) = {
	    val key :: values = splitFields(row)
	    key -> values
	  }
	  
	  val tables = (for(header :: rows <- tableLines; primaryKey :: foreignKeys = splitFields(header)) yield {
	    DataTable(primaryKey, foreignKeys, rows.map(parseRow(_)).toMap)
	  }).toList
	  
	  DataTables(tables)
	  
	}
}