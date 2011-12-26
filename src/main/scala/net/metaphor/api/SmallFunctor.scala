package net.metaphor.api

trait SmallFunctor[C <: SmallCategory[C]] extends Functor[C] with SmallHeteroFunctor[C, C] { smallFunctor =>

  
  
}