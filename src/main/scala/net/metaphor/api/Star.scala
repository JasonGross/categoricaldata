package net.metaphor.api


sealed trait Star extends FPCategory

object Star {
  def apply(n: Int): Star = apply(n, "id")
  def apply(n: Int, id: String): Star = apply(n, id, (1 to n).toList.map("attr:" + _))
  def apply(n: Int, columns: List[String]): Star = apply(n, "id", columns)
  def apply(n: Int, id: String, columns: List[String]): Star = new Star {
    val centre = Box(id)
    val leaves = columns.map(c => Box(c))
    val boxes = centre :: leaves
    val arrows = leaves map { c => Arrow(centre, c, c.label) }
  }
}
