package net.categoricaldata.load

object CSVLoader extends DataLoader {
	def fromLines(lines: Iterable[String]): DataTables = {
	  // split at empty lines
	  // in each chunk, take the first line as the keys and subsequent lines as rows
	  ???
	}
}