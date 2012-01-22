package net.categoricaldata.server.json

object PrettyPrinter {
  def apply(o: Any): String = {
    import net.liftweb.json.Printer._
    import net.liftweb.json.Printer
    import net.liftweb.json.JsonAST._
    import net.liftweb.json.Extraction._

    implicit val formats = net.liftweb.json.DefaultFormats

    Printer.pretty(render(decompose(o)))
  }
}