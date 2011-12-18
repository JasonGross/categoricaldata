package net.metaphor.api

trait Set[A] {
  def toIterable: Iterable[A]
}
trait Function[A, B] {
  def toFunction: A => B
}

trait Sets extends Category[Set[Any], Function[Any, Any]]

object Sets extends Sets {
  override def identity(set: Set[Any]) = ???
  override def source(f: Function[Any, Any]) = ???
  override def target(f: Function[Any, Any]) = ???
  override def compose(first: Function[Any, Any], second: Function[Any, Any]) = ???
}

object Colimit {
  /**
   * returns a pair (s: Set, fs: List[Function]), where s is the colimit, and fs are the functions from the objects of functor.source to the colimit
   */
  def apply[O, M, C <: FinitelyPresentedCategory[O, M, C]](functor: FunctorToSet[O, M, C]): (Set[_], List[Function[_, _]]) = {
    val diagram = functor.source

    /**
     * finds all the clumps containing an element of slice, and smushes them together
     */
    def combineClumps[A](clumps: List[List[A]], slice: List[A]): List[List[A]] = {
      val (toCombine, toLeave) = clumps.partition(_.find(a => slice.contains(a)).nonEmpty)
      toCombine.flatten :: toLeave
    }

    val initialClumps = for (
      o <- diagram.objects;
      x <- functor(o).toIterable
    ) yield List((o, x))
    val arrows = for (
      m <- diagram.generators;
      s = diagram.source(m);
      t = diagram.target(m);
      x <- functor(s).toIterable
    ) yield List((s, x), (t, functor(m).toFunction(x)))

    lazy val clumps = arrows.foldLeft(initialClumps)(combineClumps _)
    
    val resultSet = new Set[List[(O, Any)]] {
      def toIterable = clumps
    }
    val resultFunctions = diagram.objects.map { o =>
      new Function[Any, List[(O, Any)]] {
        def toFunction = { x => clumps.find(_.contains((o, x))).get }
      }
    }
    
    (resultSet, resultFunctions)
  }
}
