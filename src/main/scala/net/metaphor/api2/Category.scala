package net.metaphor.api2

trait Category { c =>
  type C <: Category
  type O
  type M[X <: O, Y <: O]

  def identity[X <: O](o: X): M[X, X]
  def source[X <: O, Y <: O](m: M[X, Y]): X
  def target[X <: O, Y <: O](m: M[X, Y]): Y
  def compose[X <: O, Y <: O, Z <: O](m1: M[X, Y], m2: M[Y, Z]): M[X, Z]

  trait FunctorsFrom extends net.metaphor.api2.FunctorsFrom {
    type SC = C
    val source = c.asInstanceOf[C]
  }
  trait FunctorsTo extends net.metaphor.api2.FunctorsTo {
    type TC = C
    val target = c.asInstanceOf[C]
  }

}
