package net.metaphor.api

trait FinitelyPresentedFunctor extends FinitelyGeneratedFunctor { fpFunctor =>
  override val source: FinitelyPresentedCategory
  override val target: FinitelyPresentedCategory

  def verifyRelations = {
    for (relation <- source.allRelations) {
      require(target.pathEquality(fpFunctor.onMorphisms(source.pathAsMorphism(relation._1)).representative, fpFunctor.onMorphisms(source.pathAsMorphism(relation._2)).representative))
    }
  }
}
