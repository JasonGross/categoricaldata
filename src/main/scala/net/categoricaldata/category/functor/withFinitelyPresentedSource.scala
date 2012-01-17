package net.categoricaldata.category.functor
import net.categoricaldata.category._
trait withFinitelyPresentedSource extends withFinitelyGeneratedSource { functor =>
  override val source: FinitelyPresentedCategory
  def verifyRelations = {
    for (relation <- source.allRelations) {
      require(functor.onMorphisms(source.pathAsMorphism(relation._1)) == functor.onMorphisms(source.pathAsMorphism(relation._2)))
    }
  }
}
