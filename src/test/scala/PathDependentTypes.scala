object Foo {
  trait Bar {

  }
}

object Main extends App {
  val foo1 = Foo
  val foo2 = foo1

  def turkle(x: foo1.Bar) {}

  turkle(new foo2.Bar {})
}