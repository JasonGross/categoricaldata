package net.metaphor.api2

object CategoriesOfSets extends Categories {
  type C = Sets
  sealed trait Sets extends Category {
    type O = Set[String]
    type M[X <: O, Y <: O] = Function[X, Y]
    
    case class Function[X <: O, Y <: O](source: X, target: Y, function: String => String)
    
    def identity[X <: O](o: X): M[X, X] = Function(o, o, { x => x })
    def source[X <: O, Y <: O](m: M[X, Y]): X = m.source
    def target[X <: O, Y <: O](m: M[X, Y]): Y = m.target
    def compose[X <: O, Y <: O, Z <: O](m1: M[X, Y], m2: M[Y, Z]): M[X, Z] = Function(m1.source, m2.target, m1.function andThen m2.function)
    
  }
  object Sets extends Sets  
}