package net.categoricaldata.category.functor.withLocallyFinitelyGeneratedSource
import net.categoricaldata.category._

trait withLocallyFinitelyGeneratedTarget extends functor.withSmallSource.withLocallyFinitelyGeneratedTarget with functor.withLocallyFinitelyGeneratedSource.withSmallTarget { lfgFunctor =>
  abstract class SliceCategory(onLeft: lfgFunctor.target.O) extends super.SliceCategory(onLeft) {
    override def objectsAtLevel(k: Int): List[ObjectRightOf] = {
      for (
        l <- (lfgFunctor.source.minimumLevel to k).toList;
        right <- lfgFunctor.source.objectsAtLevel(l);
        path <- lfgFunctor.target.wordsOfLength(k - l)(onLeft, lfgFunctor.onObjects(right))
      ) yield ObjectRightOf(right, lfgFunctor.target.pathAsMorphism(path))
    }
  }
  abstract class CosliceCategory(onRight: lfgFunctor.target.O) extends super.CosliceCategory(onRight) {
    override def objectsAtLevel(k: Int): List[ObjectLeftOf] = {
      for (
        l <- (lfgFunctor.source.minimumLevel to k).toList;
        left <- lfgFunctor.source.objectsAtLevel(l);
        path <- lfgFunctor.target.wordsOfLength(k - l)(lfgFunctor.onObjects(left), onRight)
      ) yield ObjectLeftOf(left, lfgFunctor.target.pathAsMorphism(path))
    }
  }

}
