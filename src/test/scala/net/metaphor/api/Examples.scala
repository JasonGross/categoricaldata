package net.metaphor.api

object ExampleCategory extends FPCategory {
  val Emp = Box("Emp", "an employee")
  val Dpt = Box("Dpt", "a department")

  val boxes = List(Emp, Dpt)
  val arrows = List(Arrow(Emp, Dpt, "f", "is in"), Arrow(Dpt, Emp, "g", "has as secretary"))
}

object ExampleFunctor extends FPFunctor {
  val source = ExampleCategory
  val target = ExampleCategory

  val boxMap = Map(source.Emp -> target.Dpt, source.Dpt -> target.Dpt)
  val arrowMap = { arrow: source.Arrow => target.Path(target.Dpt, Nil) }

  verify
}

object ExampleData extends InMemoryFPFunctorToSet {
  val source = ExampleCategory
  val boxData = Map(source.Emp -> List("David", "Scott"), source.Dpt -> List("mathematicians"))
  val arrowData = Map(source.arrows(0) -> List("David" -> "mathematicians", "Scott" -> "mathematicians"), source.arrows(1) -> List("mathematicians" -> "David"))
}

object Main extends App {
  ExampleFunctor(ExampleCategory.Emp)
  println(ExampleCategory.describe)
}

