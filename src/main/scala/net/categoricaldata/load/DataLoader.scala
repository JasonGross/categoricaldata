package net.categoricaldata.load
import java.io.Reader
import java.io.StringReader
import java.net.URL
import java.io.InputStreamReader
import scala.io.Source
import java.io.InputStream

trait DataLoader {
	def fromURL(URLs: String*): DataTables = fromLines(URLs.map(Source.fromURL(_).getLines).flatten)
	def fromStrings(strings: String*): DataTables = fromLines(strings.map(Source.fromString(_).getLines).flatten)
	def fromStreams(streams: InputStream*): DataTables = {
	  val tables = fromLines(streams.map(Source.fromInputStream(_).getLines).flatten)
	  for(s <- streams) s.close
	  tables
	}
	
	def fromLines(lines: Iterable[String]): DataTables
	
	def apply(URLs: String*) = fromURL(URLs:_*).toDataset
}