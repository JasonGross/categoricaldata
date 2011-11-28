package net.metaphor.api

// TODO, Set and Function really should be parametrized types.
trait Set {
  def toIterable: Iterable[Any]
}
trait Function {
  def toFunction: Any => Any
}

trait Sets extends Category[Set, Function]

object Sets extends Sets {
  def identity(set: Set) = ???
  def source(f: Function) = ???
  def target(f: Function) = ???
  def compose(first: Function, second: Function) = ???
}

trait FunctorToSet[O, M, C <: Category[O, M]] extends HeteroFunctor[O, M, C, Set, Function, Sets]

object Colimit {
  /**
   * returns a pair (s: Set, fs: List[Function]), where s is the colimit, and fs are the functions from the objects of functor.source to the colimit
   */
  def apply[O, M, C <: FinitelyPresentedCategory[O, M]](functor: FunctorToSet[O, M, C]): (Set, List[Function]) = {
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

    val clumps = arrows.foldLeft(initialClumps)(combineClumps _)
    
    val resultSet = new Set {
      def toIterable = clumps
    }
    val resultFunctions = diagram.objects.map { o =>
      new Function {
        def toFunction = { x => clumps.find(_.contains((o, x))).get }
      }
    }
    
    (resultSet, resultFunctions)
  }
}
