package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers
import net.tqft.toolkit.collections.NonStrictIterable

case class Path[O, G](source: O, target: O, morphisms: List[G]) {
  override def toString = source.toString + morphisms.mkString
}

/**
 * A LocallyFinitelyGeneratedCategory may have infinitely many objects, but each object sits at some integer level,
 * and there are only finitely many objects at each level.  Otherwise, the levels are completely ignored; in particular,
 * they do not provide a grading.
 *
 * Each pair of objects has a finite set of 'generators'. This means that every morphism between two objects
 * can be written as some composition of 'generators' between some chain of objects (with no restrictions on the levels).
 */

trait LocallyFinitelyGeneratedCategory extends SmallCategory {
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
trait FinitelyGeneratedCategory extends LocallyFinitelyGeneratedCategory { self =>
  // TODO, maybe minimumLevel actually belongs one level up; we could insist everything is bounded below.
  // In that case, we'd have to pull opposite down.
  val minimumLevel: Int
  val maximumLevel: Int

  def objects: List[O] = for (k <- (minimumLevel to maximumLevel).toList; o <- objectsAtLevel(k)) yield o

  /**
   * This returns all possible words in the generates.
   */
  def wordsOfLength(k: Int)(source: O, target: O): List[Path] = {
    val result: List[Path] = k match {
      case 0 => {
        if (source == target) {
          List(Path(source, source, Nil))
        } else {
          Nil
        }
      }
      case 1 => generators(source, target).map(generatorAsPath _)
      case _ => for (g <- generatorsFrom(source); Path(_, _, morphisms) <- wordsOfLength(k - 1)(self.target(g), target)) yield Path(source, target, g :: morphisms)
    }
    for (p <- result) {
      require(p.source == source)
      require(p.target == target)
    }
    result
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

  trait FinitelyGeneratedCategoryOver extends CategoryOver with FinitelyGeneratedFunctor {
    override val source: FinitelyGeneratedCategory
    def onGenerators(g: source.G): target.M
    override def onMorphisms(m: source.M) = {
      val start = onObjects(source.source(m))
      val morphisms = for (g <- m.representative.morphisms) yield onGenerators(g)
      target.compose(start, morphisms)
    }
  }

  trait FinitelyGeneratedFunctorOver extends FunctorOver {
    override val source: FinitelyGeneratedCategoryOver
    override val target: FinitelyGeneratedCategoryOver
    trait F extends super.F with FinitelyGeneratedFunctor
    override val functor: F
  }

  trait FinitelyGeneratedCategoriesOver extends Category { categoriesOver =>
    override type O = FinitelyGeneratedCategoryOver
    override type M = FinitelyGeneratedFunctorOver
    override def identity(f: O) = new FinitelyGeneratedFunctorOver {
      val source = f
      val target = f
      val functor = ??? // FIXME
      //new Functor.IdentityFunctor(f.category)
    }
    override def source(t: M) = t.source
    override def target(t: M) = t.target
    override def compose(m1: M, m2: M): M = new FinitelyGeneratedFunctorOver {
      val source = m1.source
      val target = m2.target
      val functor = ??? // FIXME
      //new Functor.CompositeFunctor(m1.functor, m2.functor)
    }
  }

  def finitelyGeneratedCategoriesOver: FinitelyGeneratedCategoriesOver = new FinitelyGeneratedCategoriesOver {}

  override type F <: FunctorToSet
  override type T <: NaturalTransformationToSet

  trait FunctorToSet extends super.FunctorToSet { functorToSet =>
    def onGenerators(g: G): FFunction
    override def onMorphisms(m: M) = {
      val start = onObjects(source.source(m))
      val morphisms = for (g <- m.representative.morphisms) yield onGenerators(g)
      target.compose(start, morphisms)
    }

    def colimitCoCone = colimit.initialObject
    def colimitSet = colimitCoCone.terminalSet
    def limitCone = limit.terminalObject
    def limitSet = limitCone.initialSet

    trait CoCone {
      def terminalSet: Set
      def mapToTerminalSet(o: O): FFunction
    }
    trait CoConeMap extends {
      def source: CoCone
      def target: CoCone
      def terminalMap: FFunction
    }
    trait Cone extends {
      def initialSet: Set
      def mapFromInitialSet(o: O): FFunction
    }
    trait ConeMap extends {
      def source: Cone
      def target: Cone
      def initialMap: FFunction
    }

    def limit: TerminalObject[functorToSet.Cone, functorToSet.ConeMap] = {
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
        { o: self.O => functorToSet(o).toIterable },
        { s: self.O => { t: self.O => { a: Any => for (g <- generators(s, t)) yield functorToSet(g).toFunction(a) } } })

      val resultSet = new Set {
        override def sizeIfFinite = Some(maps.size)
        override def toIterable = maps
      }
      val resultFunctions: (self.O => FFunction) = { o: self.O =>
        new FFunction {
          override def source = ???
          override def target = ???
          override def toFunction = functions(o).asInstanceOf[Any => Any]
        }
      }

      new TerminalObject[functorToSet.Cone, functorToSet.ConeMap] {
        def terminalObject = new functorToSet.Cone {
          override def initialSet = resultSet
          override def mapFromInitialSet(o: self.O) = resultFunctions(o)
        }
        def morphismFrom(other: functorToSet.Cone) = {
          new functorToSet.ConeMap {
            override val source = other
            override val target = terminalObject
            override def initialMap = ???
          }
        }
      }

    }

    def colimit: InitialObject[functorToSet.CoCone, functorToSet.CoConeMap] = {

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
        { o: self.O => functorToSet(o).toIterable },
        { s: self.O => { t: self.O => { a: Any => for (g <- generators(s, t)) yield functorToSet(g).toFunction(a) } } })

      val resultSet = new Set {
        override def sizeIfFinite = Some(clumps.size)
        override def toIterable = clumps
      }
      val resultFunctions: (self.O => FFunction) = { o: self.O =>
        new FFunction {
          override def source = ???
          override def target = ???
          override def toFunction = functions(o)
        }
      }

      new InitialObject[functorToSet.CoCone, functorToSet.CoConeMap] {
        def initialObject = new functorToSet.CoCone {
          override def terminalSet = resultSet
          override def mapToTerminalSet(o: self.O) = resultFunctions(o)
        }
        def morphismTo(other: functorToSet.CoCone) = {
          new functorToSet.CoConeMap {
            override val source = initialObject
            override val target = other
            override def terminalMap = ???
          }
        }
      }
    }

  }

  val functorsToSet: SpecializedFunctorsToSet

  class SpecializedFunctorsToSet extends super.SpecializedFunctorsToSet { functorsToSet =>
  }

}

object FinitelyGeneratedCategories {
  trait StandardFunctorsToSet { C: FinitelyGeneratedCategory =>
    val functorsToSet = new SpecializedFunctorsToSet

    override type F = FunctorToSet
    override type T = NaturalTransformationToSet

    def internalize(f: net.metaphor.api.FunctorToSet): F = new FunctorToSet {
      require(f.source == source)
      def onObjects(o: O) = f(o.asInstanceOf[f.source.O])
      def onGenerators(g: G) = f(C.generatorAsMorphism(g).asInstanceOf[f.source.M])
    }
    def internalize(t: net.metaphor.api.NaturalTransformationToSet): T = ???
  }
}

