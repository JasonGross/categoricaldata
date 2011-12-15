package net.metaphor.dsl

object Sentences2Example extends App {
  
  // OBSOLETE, ignore
  
  
  import Sentences2._

  val C = Category having Boxes("A", "B") having Arrows("A" --- "h" --> "B")
  
  val F = Functor on Boxes("A" -> "C", "B" -> "D") on Arrows(("A" --- "h" --> "B") -> ("C" --- "g" --> "D"))
  
  // FIXME this doesn't work yet :-(
//  val d = Dataset on Boxes("A" -> List("a", "aa"), "B" -> ("bb", "bbb"))
  
}