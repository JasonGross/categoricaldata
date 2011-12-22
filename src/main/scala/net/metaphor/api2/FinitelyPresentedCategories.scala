package net.metaphor.api2

object FinitelyPresentedCategories extends Categories {
  type C = FinitelyPresentedCategory
  trait FinitelyPresentedCategory extends Category {
    case class Box(label: String)
    case class Arrow(source: Box, target: Box, label: String)
    case class Path(source: Box, arrows: List[Arrow]) {
      def target = arrows.lastOption.map(_.target).getOrElse(source)
    }

    type O = Box
    type M[X <: O, Y <: O] = Path
    
    def boxes: List[Box]
    def arrows(s: Box, t: Box): List[Arrow]
    
    override def identity[X <: O](b: X) = Path(b, Nil)
    override def source[X <: O, Y <: O](m: M[X, Y]) = m.source.asInstanceOf[X]
    override def target[X <: O, Y <: O](m: M[X, Y]) = m.target.asInstanceOf[Y]
    override def compose[X <: O, Y <: O, Z <: O](m1: M[X, Y], m2: M[Y, Z]) = Path(m1.source, m1.arrows ::: m2.arrows)
  }
}