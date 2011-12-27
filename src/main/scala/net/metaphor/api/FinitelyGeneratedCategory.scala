package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers
import net.tqft.toolkit.collections.NonStrictIterable

case class Path[O, G](source: O, target: O, morphisms: List[G])

/**
 * A LocallyFinitelyGeneratedCategory may have infinitely many objects, but each object sits at some integer level,
 * and there are only finitely many objects at each level.  Otherwise, the levels are completely ignored; in particular,
 * they do not provide a grading.
 *
 * Each pair of objects has a finite set of 'generators'. This means that every morphism between two objects
 * can be written as some composition of 'generators' between some chain of objects (with no restrictions on the levels).
 */

trait LocallyFinitelyGeneratedCategory[C <: LocallyFinitelyGeneratedCategory[C]] extends SmallCategory[C] { self: C =>
  override type M = PathEquivalenceClass
  type G
  type Path = net.metaphor.api.Path[O, G]

  def generatorSource(g: G): O
  def generatorTarget(g: G): O
  
  def objectsAtLevel(k: Int): List[O]

  def generators(source: O, target: O): List[G]

  implicit def generatorAsPath(g: G) = Path(generatorSource(g), generatorTarget(g), List(g))
  implicit def pathAsMorphism(p: Path) = PathEquivalenceClass(p)
  implicit def generatorAsMorphism(g: G): M = pathAsMorphism(generatorAsPath(g))

  override def compose(m1: M, m2: M) = {
    val p1 = m1.representative
    val p2 = m2.representative
    require(p1.target == p2.source)
    PathEquivalenceClass(Path(p1.source, p2.target, p1.morphisms ::: p2.morphisms))
  }
  override def source(m: M): O = m.representative.source
  override def target(m: M): O = m.representative.target
  override def identity(o: O) = PathEquivalenceClass(Path(o, o, Nil))

  case class PathEquivalenceClass(representative: Path) {
    override def equals(other: Any) = {
      other match {
        case PathEquivalenceClass(otherRepresentative) => pathEquality(representative, otherRepresentative)
        case _ => false
      }
    }
  }

  def pathEquality(path1: Path, path2: Path): Boolean = ???

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

/**
 * A FinitelyGeneratedCategory is just a LocallyFinitelyGeneratedCategory with finitely many levels (and so finitely many objects and generators).
 */
trait FinitelyGeneratedCategory[C <: FinitelyGeneratedCategory[C]] extends LocallyFinitelyGeneratedCategory[C] { self: C =>
  // TODO, maybe minimumLevel actually belongs one level up; we could insist everything is bounded below.
  // In that case, we'd have to pull opposite down.
  val minimumLevel: Int
  val maximumLevel: Int

  def objects: List[O] = for (k <- (minimumLevel to maximumLevel).toList; o <- objectsAtLevel(k)) yield o

  /**
   * This returns all possible words in the generates.
   */
  def wordsOfLength(k: Int)(source: O, target: O): List[Path] = {
    k match {
      case 0 => List(Path(source, source, Nil))
      case 1 => generators(source, target).map(generatorAsPath _)
      case _ => for (g <- generatorsFrom(source); Path(_, _, morphisms) <- wordsOfLength(k - 1)(self.target(g), target)) yield Path(source, target, g :: morphisms)
    }
  }

  def generatorsFrom(source: O) = for (target <- objects; g <- generators(source, target)) yield g
  def generatorsTo(target: O) = for (source <- objects; g <- generators(source, target)) yield g
  def allGenerators: List[G] = for (source <- objects; target <- objects; g <- generators(source, target)) yield g

  def allWordsOfLength(k: Int): List[Path] = {
    for (s <- objects; t <- objects; w <- wordsOfLength(k)(s, t)) yield w
  }
  def words(source: O, target: O) = (for (k <- NonStrictNaturalNumbers) yield wordsOfLength(k)(source, target)).takeWhile(_.nonEmpty).flatten // TODO return immediately with a lazy iterable
  def allWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k)).takeWhile(_.nonEmpty).flatten
  def allNontrivialWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k + 1)).takeWhile(_.nonEmpty).flatten

  //  trait Opposite extends super.Opposite { opposite: C =>
  //    override val minimumLevel = self.maximumLevel
  //    override val maximumLevel = self.minimumLevel
  //  }

  //TODO: relax the return type.

  val adjoinTerminalObject: TerminalObjectAdjoined // FinitelyGeneratedCategory[C] with TerminalObject[O, M]
  val adjoinInitialObject: InitialObjectAdjoined //FinitelyGeneratedCategory[C] with InitialObject[O, M]

  trait TerminalObjectAdjoined extends FinitelyGeneratedCategory[C] with TerminalObject[O, M] { terminal: C =>
    override type O = self.O
    override type G = self.G

    def terminalObject: O
    def generatorFrom(o: O): G
    
    override def morphismFrom(o: O) = self.generatorAsMorphism(generatorFrom(o))

    override val minimumLevel = self.minimumLevel
    override val maximumLevel = self.maximumLevel + 1
    override def objectsAtLevel(k: Int) = if (k == maximumLevel) {
      List(terminalObject)
    } else {
      self.objectsAtLevel(k)
    }
    override def generators(source: O, target: O) = {
      if (target == terminalObject) {
        List(generatorFrom(source))
      } else {
        self.generators(source, target)
      }
    }
  }
  trait InitialObjectAdjoined extends FinitelyGeneratedCategory[C] with InitialObject[O, M] { initial: C =>
    override type O = self.O
    override type G = self.G

    def initialObject: O
    def generatorTo(o: O): G

    override def morphismTo(o: O) = self.generatorAsMorphism(generatorTo(o))

    override val minimumLevel = self.minimumLevel - 1
    override val maximumLevel = self.maximumLevel
    override def objectsAtLevel(k: Int) = if (k == minimumLevel) {
      List(initialObject)
    } else {
      self.objectsAtLevel(k)
    }
    override def generators(source: O, target: O) = {
      if (source == initialObject) {
        List(generatorTo(target))
      } else {
        self.generators(source, target)
      }
    }
  }
  
  
  trait FinitelyGeneratedFunctorTo[SC <: FinitelyGeneratedCategory[SC]] extends super.FunctorTo[SC]  with FunctorWithFinitelyGeneratedSource[SC, C] {
  }
  
  trait FunctorToSet extends super.FunctorToSet with FunctorWithFinitelyGeneratedSource[C, Sets] { functorToSet =>
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
      override def onGenerators(g: G): Function = {
        if (self.target(g) == adjoinTerminalObject.terminalObject) {
          mapToTerminalSet(self.source(g))
        } else {
          functorToSet(g)
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
      override def onGenerators(g: G): Function = {
        if (self.target(g) == adjoinInitialObject.initialObject) {
          mapFromInitialSet(self.source(g))
        } else {
          functorToSet(g)
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

        //        def checkMapsOutOfObject(s: self.O)(map: Map[self.O, A]) = {
        //          for(t <- map.keys)
        //        }
        //        
        //        val successiveProducts = objects.scanLeft(NonStrictIterable(Map.empty[self.O, A]))({
        //          (i,o) => for (m <- i; a <- sets(o)) yield m + (o -> a)
        //        })

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

  trait FunctorWithFinitelyGeneratedSource[SC <: FinitelyGeneratedCategory[SC], TC <: Category[TC]] { f: HeteroFunctor[SC, TC] =>
    def onGenerators(g: f.source.G): f.target.M
    override def onMorphisms(m: f.source.M) = f.target.compose(onObjects(f.source.source(m)), m.representative.morphisms.map(onGenerators _))
  }


//
//trait ConcreteFinitelyGeneratedCategory extends FinitelyGeneratedCategory[ConcreteFinitelyGeneratedCategory] {
//  type F = FunctorToSet
//  type T = NaturalTransformationToSet[FunctorToSet]
//  type CSets = FunctorsToSet
//  lazy val functorsToSet = ???
//  lazy val adjoinInitialObject = ???
//  lazy val adjoinTerminalObject = ???
//
//  override def liftFunctorToSet(f: net.metaphor.api.FunctorToSet[ConcreteFinitelyGeneratedCategory]): FunctorToSet = {
//    new FunctorToSet {
//      def onObjects(o: O) = f(o.asInstanceOf[f.source.O])
//      def onMorphisms(m: M) = f(m.asInstanceOf[f.source.M])
//    }
//  }
//  override def liftNaturalTransformationToSet(t: net.metaphor.api.NaturalTransformationToSet[ConcreteFinitelyGeneratedCategory, FunctorToSet]): NaturalTransformationToSet[FunctorToSet] = {
//    new NaturalTransformationToSet[FunctorToSet] {
//      val source = liftFunctorToSet(t.source)
//      val target = liftFunctorToSet(t.target)
//      def apply(o: O) = t(o)
//    }
//  }
//}
//
//class FinitelyGeneratedCategoryWrapper[C <: FinitelyGeneratedCategory[C]](val c: FinitelyGeneratedCategory[C]) extends ConcreteFinitelyGeneratedCategory {
//  override type O = c.O
//  override type G = c.G
//  val maximumLevel = c.maximumLevel
//  val minimumLevel = c.minimumLevel
//  def objectsAtLevel(k: Int) = c.objectsAtLevel(k)
//  def generators(s: O, t: O) = c.generators(s, t)
//  def identity(o: O) = c.identity(o)
//  def source(m: M) = c.source(m)
//  def target(m: M) = c.target(m)
//  def compose(m1: M, m2: M) = c.compose(m1, m2)
//}

trait FinitelyGeneratedCategories[C <: FinitelyGeneratedCategory[C]] /* extends Categories[O, M, C] */ { FGCAT =>
}
