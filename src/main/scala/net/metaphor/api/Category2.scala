//trait Category[C <: Category[C]] {
//  val boxes: List[Box[C]]
//  val arrows: List[Arrow[C]]
//  val constraints: List[Constraint[C]] = Nil
//
//  def describe = arrows.map(_.describe).mkString("\n")
//}
//
//case class Box[C <: Category[C]](name: String, description: String = "")
//case class Arrow[C <: Category[C]](source: Box[C], target: Box[C], label: String, description: String = "") {
//  def describe = source.description + " " + description + " " + target.description
//}
//case class Path[C <: Category[C]](initialBox: Box[C], arrows: List[Arrow[C]]) {
//  require(arrows.isEmpty || arrows.head.source == initialBox)
//  // TODO check sources and targets match
//  def finalBox = arrows.last.target
//}
//case class Constraint[C <: Category[C]](left: Path[C], right: Path[C]) {
//  // TODO check something
//}
//
//trait Functor[C <: Category[C], D <: Category[D]] {
//  val source: Category[C]
//  val target: Category[D]
//
//  val boxMap: Box[C] => Box[D]
//}
//
//class ExampleCategory extends Category[ExampleCategory] {
//  val Emp = Box[ExampleCategory]("Emp", "an employee")
//  val Dpt = Box[ExampleCategory]("Dpt", "a department")
//
//  val boxes = List(Emp, Dpt)
//  val arrows = List(Arrow(Emp, Dpt, "f", "is in"))
//}
//
//object Main extends App {
//  println(new ExampleCategory().describe)
//}
//
