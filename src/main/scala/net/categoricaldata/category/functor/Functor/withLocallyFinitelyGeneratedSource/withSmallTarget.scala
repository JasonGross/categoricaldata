package net.categoricaldata.category.functor.withLocallyFinitelyGeneratedSource
import net.categoricaldata.category._

trait withSmallTarget extends functor.withSmallSource.withSmallTarget with functor.withLocallyFinitelyGeneratedSource
trait withLocallyFinitelyGeneratedTarget extends functor.withSmallSource.withLocallyFinitelyGeneratedTarget with functor.withLocallyFinitelyGeneratedSource.withSmallTarget { lfgFunctor =>
  abstract class SliceCategory(onLeft: lfgfunctor.target.O) extends super.SliceCategory(onLeft) {
    override def objectsAtLevel(k: Int): List[ObjectRightOf] = {
      for (
        l <- (lfgfunctor.source.minimumLevel to k).toList;
        right <- lfgfunctor.source.objectsAtLevel(l);
        path <- lfgfunctor.target.wordsOfLength(k - l)(onLeft, lfgfunctor.apply(right))
      ) yield ObjectRightOf(right, lfgfunctor.target.pathAsMorphism(path))
    }
  }
  abstract class CosliceCategory(onRight: lfgfunctor.target.O) extends super.CosliceCategory(onRight) {
    override def objectsAtLevel(k: Int): List[ObjectLeftOf] = {
      for (
        l <- (lfgfunctor.source.minimumLevel to k).toList;
        left <- lfgfunctor.source.objectsAtLevel(l);
        path <- lfgfunctor.target.wordsOfLength(k - l)(lfgfunctor.apply(left), onRight)
      ) yield ObjectLeftOf(left, lfgfunctor.target.pathAsMorphism(path))
    }
  }

}
trait withFinitelyGeneratedTarget extends functor.withSmallSource.withFinitelyGeneratedTarget with functor.withLocallyFinitelyGeneratedSource.withLocallyFinitelyGeneratedTarget
trait withFinitelyPresentedTarget extends functor.withSmallSource.withFinitelyPresentedTarget with functor.withLocallyFinitelyGeneratedSource.withFinitelyGeneratedTarget
