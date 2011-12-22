package net.metaphor.api


trait Foo {
  trait Bar {
    
  }
}

object Main {
  val foo1 = new Foo { }
  val foo2 = foo1
  
  trait one {
	  def turkle(x: foo1.Bar) { }
  }
  
//  turkle(new foo2.Bar { })
}