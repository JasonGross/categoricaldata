package net.metaphor.api

trait Set {
  def toIterable: Iterable[Any]
  def identity: Function = IdentityFunction(this)
}
trait Function {
  def toFunction: Any => Any
}
case class IdentityFunction[A](set: Set) extends Function {
  override def toFunction = { a: Any => a }
}

trait Sets extends Category[Set, Function, Sets]

object Sets extends Sets {
  override def identity(set: Set) = ???
  override def source(f: Function) = ???
  override def target(f: Function) = ???
  override def compose(first: Function, second: Function) = ???
}

object Colimit {
  /**
   * returns a pair (s: Set, fs: List[Function]), where s is the colimit, and fs are the functions from the objects of functor.source to the colimit
   */
  def apply[O, M, C <: FinitelyPresentedCategory[O, M, C]](functor: FunctorToSet[O, M, C]): (Set, List[Function]) = {
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
