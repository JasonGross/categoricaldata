package net.metaphor.api

trait LeftAdjoint { functor: Functor =>
  def rightAdjoint: RightAdjoint
  def rightUnit: NaturalTransformation
  def rightCounit: NaturalTransformation
}

trait RightAdjoint { functor: Functor =>
  def leftAdjoint: LeftAdjoint
  def leftUnit: NaturalTransformation
  def leftCounit: NaturalTransformation
}
