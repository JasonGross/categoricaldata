package net.categoricaldata.category

trait Equivalence { equivalence =>
	val source: Category
	val target: Category
	
	trait Source2Target extends Functor {
	  override val source: equivalence.source.type= equivalence.source
	  override val target: equivalence.target.type= equivalence.target
	}
	trait Target2Source extends Functor {
	  override val source: equivalence.target.type= equivalence.target
	  override val target: equivalence.source.type= equivalence.source
	}
  
	val functor: Source2Target
	val inverse: Target2Source
	
	val sourceEquivalence: NaturalTransformation
	val targetEquivalence: NaturalTransformation
}