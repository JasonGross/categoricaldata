package net.metaphor.api2

// any old functor
trait Functor {
  type SC <: Category
  type TC <: Category

  val source: SC
  val target: TC

  type SO = source.O
  type TO = target.O
  type SM[X <: SO, Y <: SO] = source.M[X, Y]
  type TM[X <: TO, Y <: TO] = target.M[X, Y]
  
  final def apply(o: SO): TO = onObjects(o)
  final def apply[X <:SO, Y <: SO](m: SM[X, Y])(implicit d: DummyImplicit): TM[TO, TO] = onMorphisms(m)

  def onObjects(o: SO): TO
  def onMorphisms[X <: SO, Y <: SO](m: SM[X, Y]): TM[TO, TO]
}
