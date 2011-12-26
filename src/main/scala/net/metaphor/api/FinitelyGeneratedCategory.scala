package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait LocallyFinitelyGeneratedCategory[C <: LocallyFinitelyGeneratedCategory[C]] extends SmallCategory[C] { self: C =>
  def objectsAtLevel(k: Int): List[O]

  def generators(source: O, target: O): List[M]

  //  trait Opposite { opposite: C =>
  //    override type O = self.O
  //    override type M = self.M
  //
  //    // reverse all the levels!
  //    override def objectsAtLevel(k: Int) = self.objectsAtLevel(-k)
  //    override def generators(source: O, target: O) = self.generators(target, source)
  //
  //    override def source(m: M) = self.target(m)
  //    override def target(m: M) = self.source(m)
  //    override def compose(m1: M, m2: M) = self.compose(m2, m1)
  //  }

  /**
   * Implementing opposite should be easy; generally just write "def opposite = new C with Opposite", replacing C as appropriate.
   */
  //  def opposite: C
}

trait FinitelyGeneratedCategory[C <: FinitelyGeneratedCategory[C]] extends LocallyFinitelyGeneratedCategory[C] { self: C =>
  // TODO, maybe minimumLevel actually belongs one level up; we could insist everything is bounded below.
  // In that case, we'd have to pull opposite down.
  val minimumLevel: Int
  val maximumLevel: Int

  def objects: List[O] = for (k <- (minimumLevel to maximumLevel).toList; o <- objectsAtLevel(k)) yield o

  def wordsOfLength(k: Int)(source: O, target: O): List[M] = {
    k match {
      case 0 => List(identity(source))
      case 1 => generators(source, target)
      case _ => for (g <- generatorsTo(target); w <- wordsOfLength(k - 1)(source, self.source(g))) yield compose(w, g)
    }
  }

  def generatorsFrom(source: O) = for (target <- objects; g <- generators(source, target)) yield g
  def generatorsTo(target: O) = for (source <- objects; g <- generators(source, target)) yield g
  def allGenerators: List[M] = for (source <- objects; target <- objects; g <- generators(source, target)) yield g

  def allWordsOfLength(k: Int): List[M] = {
    for (s <- objects; t <- objects; w <- wordsOfLength(k)(s, t)) yield w
  }
  def words(source: O, target: O) = (for (k <- NonStrictNaturalNumbers) yield wordsOfLength(k)(source, target)).takeWhile(_.nonEmpty).flatten
  def allWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k)).takeWhile(_.nonEmpty).flatten
  def allNontrivialWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k + 1)).takeWhile(_.nonEmpty).flatten

  //  trait Opposite extends super.Opposite { opposite: C =>
  //    override val minimumLevel = self.maximumLevel
  //    override val maximumLevel = self.minimumLevel
  //  }

  val adjoinTerminalObject: WithTerminalObject[C]
  val adjoinInitialObject: WithInitialObject[C]

  trait WithTerminalObject[D <: FinitelyGeneratedCategory[D]] extends FinitelyGeneratedCategory[D] with TerminalObject[O, M] { terminal: D =>
    override type O = self.O
    override type M = self.M

    def terminalObject: O
    def morphismFrom(o: O): M

    override val minimumLevel = self.minimumLevel
    override val maximumLevel = self.maximumLevel + 1
    override def objectsAtLevel(k: Int) = if (k == maximumLevel) {
      List(terminalObject)
    } else {
      self.objectsAtLevel(k)
    }
    override def generators(source: O, target: O) = {
      if (target == terminalObject) {
        List(morphismFrom(source))
      } else {
        self.generators(source, target)
      }
    }
  }
  trait WithInitialObject[D <: FinitelyGeneratedCategory[D]] extends FinitelyGeneratedCategory[D] with InitialObject[O, M] { initial: D =>
    override type O = self.O
    override type M = self.M

    def initialObject: O
    def morphismTo(o: O): M

    override val minimumLevel = self.minimumLevel - 1
    override val maximumLevel = self.maximumLevel
    override def objectsAtLevel(k: Int) = if (k == minimumLevel) {
      List(initialObject)
    } else {
      self.objectsAtLevel(k)
    }
    override def generators(source: O, target: O) = {
      if (source == initialObject) {
        List(morphismTo(target))
      } else {
        self.generators(source, target)
      }
    }
  }

  trait FunctorToSet extends super.FunctorToSet { functorToSet =>
    def colimit = functorsToSet.colimit(functorToSet)
    def colimitFunctor = colimit.initialObject.asInstanceOf[adjoinTerminalObject.FunctorToSet]
    def colimitSet = {
      val c = colimitFunctor
      c(c.source.asInstanceOf[TerminalObject[O, M]].terminalObject)
    }
    def limit = functorsToSet.limit(functorToSet)
    def limitFunctor = limit.terminalObject.asInstanceOf[adjoinInitialObject.FunctorToSet]
    def limitSet = {
      val c = limitFunctor
      c(c.source.asInstanceOf[InitialObject[O, M]].initialObject)
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
    trait Cone extends adjoinInitialObject.FunctorToSet {
      def initialSet: Set
      def mapFromInitialSet(o: O): Function

      override def onObjects(o: O): Set = {
        if (o == adjoinInitialObject.initialObject) {
          initialSet
        } else {
          functorToSet(o)
        }
      }
      override def onMorphisms(m: M): Function = {
        if (self.target(m) == adjoinInitialObject.initialObject) {
          mapFromInitialSet(self.source(m))
        } else {
          functorToSet(m)
        }
      }
    }
    trait ConeMap extends adjoinInitialObject.NaturalTransformationToSet[Cone] {
      def initialMap: Function

      override def apply(o: O) = {
        if (o == adjoinInitialObject.initialObject) {
          initialMap
        } else {
          functorToSet(o).identity
        }
      }
    }

  }

  // Contrary to appearance, these definitions are *not* redundant with those in SmallCategory.
  // Since then, we've further specialized FunctorToSet, etc., and these definitions further constrain F, T and CSets.
  override type F <: FunctorToSet
  override type T <: NaturalTransformationToSet[F]
  override type CSets <: FunctorsToSet
  val functorsToSet: CSets

  class FunctorsToSet extends super.FunctorsToSet { functorsToSet: CSets =>

    def limit(functor: self.FunctorToSet): TerminalObject[functor.Cone, functor.ConeMap] = {

      // this is where all the work happens.
      def concreteLimit[A](objects: Iterable[self.O], sets: self.O => Iterable[A], functions: self.O => (self.O => (A => Iterable[A]))): (Iterable[self.O => A], self.O => ((self.O => A) => A)) = {

        val resultMaps = ???
        def resultFunctions(o: self.O)(map: self.O => A) = map(o)

        (resultMaps, resultFunctions _)
      }

      val (maps, functions) = concreteLimit(
        objects,
        { o: self.O => functor(o).toIterable },
        { s: self.O => { t: self.O => { a: Any => for (g <- generators(s, t)) yield functor(g).toFunction(a) } } })

      val resultSet = new Set {
        override def toIterable = maps
      }
      val resultFunctions: (self.O => Function) = { o: self.O =>
        new Function {
          override def toFunction = functions(o).asInstanceOf[Any => Any]
        }
      }

      new TerminalObject[functor.Cone, functor.ConeMap] {
        def terminalObject = new functor.Cone {
          override def initialSet = resultSet
          override def mapFromInitialSet(o: self.O) = resultFunctions(o)
        }
        def morphismFrom(other: functor.Cone) = {
          new functor.ConeMap {
            override val source = other
            override val target = terminalObject
            override def initialMap = ???
          }
        }
      }

    }

    def colimit(functor: self.FunctorToSet): InitialObject[functor.CoCone, functor.CoConeMap] = {

      // This is where all the work happens.
      def concreteColimit[A](objects: Iterable[self.O], sets: self.O => Iterable[A], functions: self.O => (self.O => (A => Iterable[A]))): (Iterable[List[(self.O, A)]], self.O => (A => List[(self.O, A)])) = {
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
        def resultFunctions(o: self.O)(a: A) = resultClumps.find(_.contains((o, a))).get

        (resultClumps, resultFunctions _)
      }

      val (clumps, functions) = concreteColimit(
        objects,
        { o: self.O => functor(o).toIterable },
        { s: self.O => { t: self.O => { a: Any => for (g <- generators(s, t)) yield functor(g).toFunction(a) } } })

      val resultSet = new Set {
        override def toIterable = clumps
      }
      val resultFunctions: (self.O => Function) = { o: self.O =>
        new Function {
          override def toFunction = functions(o)
        }
      }

      new InitialObject[functor.CoCone, functor.CoConeMap] {
        def initialObject = new functor.CoCone {
          override def terminalSet = resultSet
          override def mapToTerminalSet(o: self.O) = resultFunctions(o)
        }
        def morphismTo(other: functor.CoCone) = {
          new functor.CoConeMap {
            override val source = initialObject
            override val target = other
            override def terminalMap = ???
          }
        }
      }
    }

  }

}

trait ConcreteFinitelyGeneratedCategory extends FinitelyGeneratedCategory[ConcreteFinitelyGeneratedCategory] {
  type F = FunctorToSet
  type T = NaturalTransformationToSet[FunctorToSet]
  type CSets = FunctorsToSet
  lazy val functorsToSet = ???
  lazy val adjoinInitialObject = ???
  lazy val adjoinTerminalObject = ???

  override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[ConcreteFinitelyGeneratedCategory]): FunctorToSet = {
    new FunctorToSet {
      def onObjects(o: O) = f(o.asInstanceOf[f.source.O])
      def onMorphisms(m: M) = f(m.asInstanceOf[f.source.M])
    }
  }
  override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[ConcreteFinitelyGeneratedCategory, FunctorToSet]): NaturalTransformationToSet[FunctorToSet] = {
    new NaturalTransformationToSet[FunctorToSet] {
      val source = liftFunctorToSet(t.source)
      val target = liftFunctorToSet(t.target)
      def apply(o: O) = t(o)
    }
  }
}

class FinitelyGeneratedCategoryWrapper[C <: FinitelyGeneratedCategory[C]](val c: FinitelyGeneratedCategory[C]) extends ConcreteFinitelyGeneratedCategory {
  override type O = c.O
  override type M = c.M
  val maximumLevel = c.maximumLevel
  val minimumLevel = c.minimumLevel
  def objectsAtLevel(k: Int) = c.objectsAtLevel(k)
  def generators(s: O, t: O) = c.generators(s, t)
  def identity(o: O) = c.identity(o)
  def source(m: M) = c.source(m)
  def target(m: M) = c.target(m)
  def compose(m1: M, m2: M) = c.compose(m1, m2)
}

trait FinitelyGeneratedCategories[C <: FinitelyGeneratedCategory[C]] /* extends Categories[O, M, C] */ { FGCAT =>
}
