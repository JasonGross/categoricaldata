package net.metaphor.api2


// any old natural transformation
trait NaturalTransformation {
  val sourceAndTargetCategories: Functors
  val source: sourceAndTargetCategories.Functor
  val target: sourceAndTargetCategories.Functor

  type SO = sourceAndTargetCategories.source.O
  type TO = sourceAndTargetCategories.target.O
  type TM[X <: TO, Y <: TO] = sourceAndTargetCategories.target.M[X, Y]

  // gosh, wouldn't it be nice if we could have the type system ensure this morphism was really a morphism between source(o) and target(o)
  // ... or is that just going too far?
  def apply(o: SO): TM[TO, TO]
}

// TODO various specializations constraining the source and target, as per Functors vs Functor