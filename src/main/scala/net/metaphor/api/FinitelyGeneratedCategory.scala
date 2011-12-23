package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait LocallyFinitelyGeneratedCategory[O, M, C <: LocallyFinitelyGeneratedCategory[O, M, C]] extends Category[O, M, C] { self: C =>
  def objectsAtLevel(k: Int): List[O]

  def generators(source: O, target: O): List[M]
  def generatorsFrom(source: O): List[M]
  def generatorsTo(source: O): List[M]

  def wordsOfLength(k: Int)(source: O, target: O): List[M] = {
    k match {
      case 0 => List(identity(source))
      case 1 => generators(source, target)
      case _ => for (g <- generatorsTo(target); w <- wordsOfLength(k - 1)(source, self.source(g))) yield compose(w, g)
    }
  }

  trait Opposite { opposite: C =>
    // reverse all the levels!
    override def objectsAtLevel(k: Int) = self.objectsAtLevel(-k)
    override def generators(source: O, target: O) = self.generators(target, source)

    override def source(m: M) = self.target(m)
    override def target(m: M) = self.source(m)
    override def compose(m1: M, m2: M) = self.compose(m2, m1)
  }

  /**
   * Implementing opposite should be easy; generally just write "def opposite = new C with Opposite", replacing C as appropriate.
   */
  def opposite: C
}

trait FinitelyGeneratedCategory[O, M, C <: FinitelyGeneratedCategory[O, M, C]] extends LocallyFinitelyGeneratedCategory[O, M, C] { self: C =>
  // TODO, maybe minimumLevel actually belongs one level up; we could insist everything is bounded below.
  // In that case, we'd have to pull opposite down.
  val minimumLevel: Int
  val maximumLevel: Int

  def objects: List[O] = for (k <- (minimumLevel to maximumLevel).toList; o <- objectsAtLevel(k)) yield o

  def generatorsFrom(source: O) = for (target <- objects; g <- generators(source, target)) yield g
  def generatorsTo(target: O) = for (source <- objects; g <- generators(source, target)) yield g
  def allGenerators: List[M] = for (source <- objects; target <- objects; g <- generators(source, target)) yield g

  def allWordsOfLength(k: Int): List[M] = {
    for (s <- objects; t <- objects; w <- wordsOfLength(k)(s, t)) yield w
  }
  def words(source: O, target: O) = (for (k <- NonStrictNaturalNumbers) yield wordsOfLength(k)(source, target)).takeWhile(_.nonEmpty).flatten
  def allWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k)).takeWhile(_.nonEmpty).flatten
  def allNontrivialWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k + 1)).takeWhile(_.nonEmpty).flatten

  trait Opposite extends super.Opposite { opposite: C =>
    override val minimumLevel = self.minimumLevel
    override val maximumLevel = self.maximumLevel
  }
  
  trait WithTerminalObject extends FinitelyGeneratedCategory[O, M, C] with TerminalObject[O, M] { self: C => }
  val adjoinTerminalObject: WithTerminalObject

  trait FunctorToSet extends super.FunctorToSet { functorToSet =>
    def colimit = functorsToSet.colimit(functorToSet)
    def colimitFunctor = colimit.initialObject.asInstanceOf[adjoinTerminalObject.FunctorToSet]
    def colimitSet = {
      val c = colimitFunctor
      c(c.source.asInstanceOf[TerminalObject[O, M]].terminalObject)
    }

    trait CoCone extends adjoinTerminalObject.FunctorToSet {
      def terminalSet: Set
      def mapToTerminalSet(o: O): Function

      override def onObjects(o: O): Set = {
        if (o == adjoinTerminalObject.terminalObject) {
          terminalSet
        } else {
          functorToSet(o)
        }
      }
      override def onMorphisms(m: M): Function = {
        if (self.target(m) == adjoinTerminalObject.terminalObject) {
          mapToTerminalSet(self.source(m))
        } else {
          functorToSet(m)
        }
      }
    }
    trait CoConeMap extends adjoinTerminalObject.NaturalTransformationToSet[CoCone] {
      def terminalMap: Function

      override def apply(o: O) = {
        if (o == adjoinTerminalObject.terminalObject) {
          terminalMap
        } else {
          functorToSet(o).identity
        }
      }
    }

  }

    // TODO Lift these higher? Tried once, and ran into some trouble; making functor categories have these types gets confusing fast.
  // Alternatively, we should drop FunctorToSet stuff down below the most general Category level.
  type F <: FunctorToSet
  type T <: NaturalTransformationToSet[F]
  type CSets <: FunctorsToSet[F, T, CSets]

  def lift(f: FunctorToSet): F
  def lift(t: NaturalTransformationToSet[F]): T

  val functorsToSet: CSets

  abstract class FunctorsToSet[F <: FunctorToSet, T <: NaturalTransformationToSet[F], FC <: FunctorsToSet[F, T, FC]] extends super.FunctorsToSet[F, T, FC] { functorsToSet: FC =>

    def colimit(functor: self.FunctorToSet): InitialObject[functor.CoCone, functor.CoConeMap] = {

      // This is where all the work happens.
      def concreteColimit[A](objects: Iterable[O], sets: O => Iterable[A], functions: O => (O => (A => Iterable[A]))): (Iterable[List[(O, A)]], O => (A => List[(O, A)])) = {
        /**
         * finds all the clumps containing an element of slice, and smushes them together
         */
        def combineClumps[B](clumps: Iterable[List[B]], slice: List[B]): Iterable[List[B]] = {
          val (toCombine, toLeave) = clumps.partition(_.find(a => slice.contains(a)).nonEmpty)
          toLeave ++ List(toCombine.flatten.toList)
        }
        val initialClumps = for (o <- objects; x <- sets(o)) yield List((o, x))
        val arrows = for (s <- objects; x <- sets(s); t <- objects; y <- functions(s)(t)(x)) yield List((s, x), (t, y))

        val resultClumps = arrows.foldLeft(initialClumps)(combineClumps _)
        def resultFunctions(o: O)(a: A) = resultClumps.find(_.contains((o, a))).get

        (resultClumps, resultFunctions _)
      }

      val (clumps, functions) = concreteColimit(
        objects,
        { o: O => functor(o).toIterable },
        { s: O => { t: O => { a: Any => for (g <- generators(s, t)) yield functor(g).toFunction(a) } } })

      val resultSet = new Set {
        override def toIterable = clumps
      }
      val resultFunctions: (O => Function) = { o: O =>
        new Function {
          override def toFunction = functions(o)
        }
      }

      new InitialObject[functor.CoCone, functor.CoConeMap] {
        def initialObject = new functor.CoCone {
          override def terminalSet = resultSet
          override def mapToTerminalSet(o: O) = resultFunctions(o)
        }
        def morphismTo(other: functor.CoCone) = {
          new functor.CoConeMap {
            override def source = initialObject
            override def target = other
            override def terminalMap = ???
          }
        }
      }
    }

  }

}

trait FinitelyGeneratedCategories[O, M, C <: FinitelyGeneratedCategory[O, M, C]] /* extends Categories[O, M, C] */ { FGCAT =>
}
