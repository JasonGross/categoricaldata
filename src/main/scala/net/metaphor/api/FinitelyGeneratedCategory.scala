package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers
import net.tqft.toolkit.collections.NonStrictIterable

case class Path[O, G](source: O, target: O, morphisms: List[G]) {
  if (morphisms.isEmpty) require(source == target)
  
  def length = morphisms.size
  def andThen(path: Path[O, G]) = {
    require(target == path.source)
    Path(source, path.target, morphisms ::: path.morphisms)
  }
  
  override def toString = {
    val afterFirstQuote = """".*"( --- ".*" --> ".*")""".r
    source.toString + (for (m <- morphisms; s = m.toString) yield afterFirstQuote.unapplySeq(s).get.head).mkString
  }
}

/**
 * A LocallyFinitelyGeneratedCategory may have infinitely many objects, but each object sits at some integer level,
 * and there are only finitely many objects at each level.  Otherwise, the levels are completely ignored; in particular,
 * they do not provide a grading.
 *
 * Each pair of objects has a finite set of 'generators'. This means that every morphism between two objects
 * can be written as some composition of 'generators' between some chain of objects (with no restrictions on the levels).
 */

trait LocallyFinitelyGeneratedCategory extends SmallCategory { lfgCategory =>
  override type M = PathEquivalenceClass
  type G
  type Path = net.metaphor.api.Path[O, G]

  protected implicit def path2RichPath(path: Path) = new RichPath(path)
  protected class RichPath(path: Path) {
    def subpath(i: Int, j: Int) = {
      val morphisms = path.morphisms.slice(i, j)
      val (source, target) = if(morphisms.isEmpty) {
        ???
      } else {
        (generatorSource(morphisms.head), generatorTarget(morphisms.last))
      }
      Path(source, target, morphisms)
    }
  }
  
  def generatorSource(g: G): O
  def generatorTarget(g: G): O

  def objectsAtLevel(k: Int): List[O]
  def objectSet: FSet = {
    new FSet {
      def toIterable = NonStrictNaturalNumbers.flatMap(x => Set(x, -x)).flatMap(x => objectsAtLevel(x))
      def sizeIfFinite = None
    }
  }

  def generators(source: O, target: O): List[G]

  implicit def generatorAsPath(g: G) = Path(generatorSource(g), generatorTarget(g), List(g))
  implicit def pathAsMorphism(p: Path) = PathEquivalenceClass(p)
  implicit def generatorAsMorphism(g: G): M = pathAsMorphism(generatorAsPath(g))

  override def compose(m1: M, m2: M) = PathEquivalenceClass(m1.representative andThen m2.representative)
  override def source(m: M): O = m.representative.source
  override def target(m: M): O = m.representative.target
  override def identity(o: O) = PathEquivalenceClass(Path(o, o, Nil))

  case class PathEquivalenceClass(representative: Path) {
    // sanity check
//    representative.morphisms.headOption.map(g => require(generatorSource(g) == representative.source))
//    representative.morphisms.lastOption.map(g => require(generatorTarget(g) == representative.target))
//    if (representative.morphisms.nonEmpty) {
//      for ((a, b) <- representative.morphisms zip representative.morphisms.tail) {
//        require(generatorTarget(a) == generatorSource(b))
//      }
//    }
    override def equals(other: Any) = {
      other match {
        case PathEquivalenceClass(otherRepresentative) => pathEquality(representative, otherRepresentative)
        case _ => false
      }
    }
    override def toString = representative.toString
  }

  def pathEquality(path1: Path, path2: Path): Boolean = ??? //???

  trait OppositeLocallyFinitelyGeneratedCategory extends LocallyFinitelyGeneratedCategory { opposite =>
    override type O = lfgCategory.O

    def reverseGenerator(g: lfgCategory.G): opposite.G
    def unreverseGenerator(g: opposite.G): lfgCategory.G

    def reverse(m: lfgCategory.M): opposite.M = m match {
      case lfgCategory.PathEquivalenceClass(Path(source, target, generators)) => PathEquivalenceClass(Path(target, source, generators.reverse.map(reverseGenerator(_))))
    }
    def unreverse(m: opposite.M): lfgCategory.M = m match {
      case PathEquivalenceClass(Path(source, target, generators)) => lfgCategory.PathEquivalenceClass(Path(target, source, generators.reverse.map(unreverseGenerator(_))))
    }
    
    // reverse all the levels!
    override def objectsAtLevel(k: Int) = lfgCategory.objectsAtLevel(-k)
    override def generators(source: O, target: O) = lfgCategory.generators(target, source).map(reverseGenerator(_))

    override def generatorSource(g: opposite.G) = lfgCategory.generatorTarget(unreverseGenerator(g))
    override def generatorTarget(g: opposite.G) = lfgCategory.generatorSource(unreverseGenerator(g))
  }
}

/**
 * A FinitelyGeneratedCategory is just a LocallyFinitelyGeneratedCategory with finitely many levels (and so finitely many objects and generators).
 */
trait FinitelyGeneratedCategory extends LocallyFinitelyGeneratedCategory { fgCategory =>
  // TODO, maybe minimumLevel actually belongs one level up; we could insist everything is bounded below.
  // In that case, we'd have to pull opposite down.
  val minimumLevel: Int
  val maximumLevel: Int

  lazy val objects: List[O] = for (k <- (minimumLevel to maximumLevel).toList; o <- objectsAtLevel(k)) yield o
  override def objectSet: FSet = new FSet {
    def toIterable = objects
    lazy val sizeIfFinite = Some(toIterable.size)
  }

  /**
   * This returns all possible words in the generators.
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
      case _ => for (g <- generatorsFrom(source); Path(_, _, morphisms) <- wordsOfLength(k - 1)(fgCategory.target(g), target)) yield Path(source, target, g :: morphisms)
    }
    for (p <- result) {
      require(p.source == source)
      require(p.target == target)
    }
    result
  }
  def wordsUpToLength(k: Int)(source: O, target: O): List[Path] = for(n <- (0 to k).toList; w <- wordsOfLength(n)(source, target)) yield w

  def generatorsFrom(source: O) = for (target <- objects; g <- generators(source, target)) yield g
  def generatorsTo(target: O) = for (source <- objects; g <- generators(source, target)) yield g
  def allGenerators: List[G] = for (source <- objects; target <- objects; g <- generators(source, target)) yield g

  def allWordsOfLength(k: Int): List[Path] = {
    for (s <- objects; t <- objects; w <- wordsOfLength(k)(s, t)) yield w
  }
  def allWordsUpToLength(k: Int): List[Path] = for(n <- (0 to k).toList; w <- allWordsOfLength(n)) yield w
  def words(source: O, target: O) = (for (k <- NonStrictNaturalNumbers) yield wordsOfLength(k)(source, target)).takeWhile(_.nonEmpty).flatten // TODO return immediately with a lazy iterable
  def allWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k)).takeWhile(_.nonEmpty).flatten
  def allNontrivialWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k + 1)).takeWhile(_.nonEmpty).flatten

  trait OppositeFinitelyGeneratedCategory extends FinitelyGeneratedCategory with OppositeLocallyFinitelyGeneratedCategory { opposite =>
    override val minimumLevel = fgCategory.maximumLevel
    override val maximumLevel = fgCategory.minimumLevel
  }

  class ConcreteOpposite extends OppositeFinitelyGeneratedCategory with FinitelyGeneratedCategories.StandardFunctorsToSet {
    override type G = OppositeGenerator
    case class OppositeGenerator(g: fgCategory.G)
    override def reverseGenerator(g: fgCategory.G) = OppositeGenerator(g)
    override def unreverseGenerator(g: OppositeGenerator) = g.g
  }

  lazy val opposite: OppositeFinitelyGeneratedCategory = new ConcreteOpposite

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
      val functor = ??? //???
    }
    override def source(t: M) = t.source
    override def target(t: M) = t.target
    override def compose(m1: M, m2: M): M = new FinitelyGeneratedFunctorOver {
      val source = m1.source
      val target = m2.target
      val functor = ??? //???
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

    lazy val totalSet = new FiniteFSet {
      def toIterable = for(o <- objectSet.toIterable; o2 = o.asInstanceOf[O]; x <- functorToSet(o2).toIterable) yield (o, x)
    }
    
    class Section(m: O => Any) extends FFunction {
      override val source = fgCategory.objectSet
      override val target = totalSet
      override val toFunction = m.asInstanceOf[Any => Any]
    }
    
    def colimitCoCone = colimit.initialObject
    def colimitSet = colimitCoCone.terminalSet
    def limitCone = limit.terminalObject
    def limitSet = limitCone.initialSet

    trait CoCone {
      val terminalSet: FSet
      abstract class coConeFunction(o: O) extends FFunction {
        override val source = functorToSet(o)
        override val target = terminalSet
      }
      def mapToTerminalSet(o: O): coConeFunction
    }
    trait CoConeMap extends { coConeMap =>
      val source: CoCone
      val target: CoCone
      trait terminalFunction extends FFunction {
        override val source = coConeMap.source.terminalSet
        override val target = coConeMap.target.terminalSet
      }
      val terminalMap: terminalFunction
    }
    trait Cone extends {
      val initialSet: FSet
      abstract class coneFunction(o: O) extends FFunction {
        override val source = initialSet
        override val target = functorToSet(o)
      }
      def mapFromInitialSet(o: O): coneFunction
    }
    trait ConeMap extends { coneMap =>
      val source: Cone
      val target: Cone
      trait initialFunction extends FFunction {
        override val source = coneMap.source.initialSet
        override val target = coneMap.target.initialSet
      }
      val initialMap: initialFunction
    }

    lazy val limit: TerminalObject[functorToSet.Cone, functorToSet.ConeMap] = {
      // this is where all the work happens.
      def concreteLimit[A](objects: Iterable[fgCategory.O], sets: fgCategory.O => Iterable[A], functions: fgCategory.O => (fgCategory.O => (A => Set[A]))): (Iterable[fgCategory.O => A], fgCategory.O => ((fgCategory.O => A) => A)) = {
        /**
         *  @returns sets(o2) if there are actually no morphisms from o1 to o2
         * 			 None if there are several morphisms, with different images on a
         * 			 Some(b) is all morphisms send a to b.
         */
        def functionsCompatibleResults(o1: fgCategory.O, o2: fgCategory.O)(a: A, constrainedValue: Option[A] = None): Iterable[A] = {
          val results = functions(o1)(o2)(a).toList
          results match {
            case h :: t => {
              val candidate = t.foldLeft[Option[A]](Some(h))({ case (Some(b), c) if b == c => Some(b); case _ => None })
              constrainedValue match {
                case None => candidate
                case Some(value) => candidate.filter(_ == value)
              }
            }
            case Nil => constrainedValue.map(List(_)).getOrElse(sets(o2))
          }
        }

        case class Intermediate(processedObjects: List[fgCategory.O], processedPairs: List[(fgCategory.O, fgCategory.O)], maps: Iterable[Map[fgCategory.O, A]]) {
          private def productWith(o: fgCategory.O) = {
            if (processedObjects.contains(o)) this
            else {
              Intermediate(o :: processedObjects, processedPairs, for (m <- maps; a <- sets(o)) yield m + (o -> a))
            }
          }
          def processPair(pair: (fgCategory.O, fgCategory.O)): Intermediate = {
            if (processedObjects.contains(pair._1)) {
              if (processedObjects.contains(pair._2)) {
                val newMaps = for (m <- maps; cr <- functionsCompatibleResults(pair._1, pair._2)(m(pair._1), Some(m(pair._2)))) yield m
                Intermediate(processedObjects, pair :: processedPairs, newMaps)
              } else {
                val newMaps = for (m <- maps; cr <- functionsCompatibleResults(pair._1, pair._2)(m(pair._1))) yield m + (pair._2 -> cr)
                Intermediate(pair._2 :: processedObjects, pair :: processedPairs, newMaps)
              }
            } else {
              productWith(pair._1).processPair(pair)
            }
          }
        }
        val start = Intermediate(Nil, Nil, NonStrictIterable(Map.empty[fgCategory.O, A]))

        val finish = (for (o1 <- objects; o2 <- objects) yield (o1, o2)).foldLeft(start)({ _.processPair(_) })

        def resultFunctions(o: fgCategory.O)(map: fgCategory.O => A) = map(o)

        (finish.maps, resultFunctions _)
      }

      val (maps, functions) = concreteLimit(
        objects,
        { o: fgCategory.O => functorToSet(o).toIterable },
        { s: fgCategory.O => { t: fgCategory.O => { a: Any => (for (g <- generators(s, t)) yield functorToSet(g).toFunction(a)).toSet } } })

      val resultSet = new FSet {
        override  def sizeIfFinite = Some(maps.size)
        override def toIterable = maps.map(new Section(_))
      }

      new TerminalObject[functorToSet.Cone, functorToSet.ConeMap] {
        val terminalObject = new functorToSet.Cone {
          override val initialSet = resultSet
          override def mapFromInitialSet(o: fgCategory.O) = new coneFunction(o) {
            override def toFunction = ({s:Section => s.toFunction.asInstanceOf[fgCategory.O => Any]} andThen functions(o)).asInstanceOf[Any => Any]
          }
        }
        def morphismFrom(other: functorToSet.Cone) = {
          new functorToSet.ConeMap {
            override val source = other
            override val target = terminalObject
            override val initialMap = new initialFunction {
              override def toFunction = { x: Any => new Section({ o: fgCategory.O => other.mapFromInitialSet(o).toFunction(x) }) }
            }
          }
        }
      }
    }

    lazy val colimit: InitialObject[functorToSet.CoCone, functorToSet.CoConeMap] = {

      // This is where all the work happens.
      def concreteColimit[A](objects: Iterable[fgCategory.O], sets: fgCategory.O => Iterable[A], functions: fgCategory.O => (fgCategory.O => (A => Set[A]))): (Iterable[Set[(fgCategory.O, A)]], fgCategory.O => (A => Set[(fgCategory.O, A)])) = {
        /**
         * finds all the clumps containing an element of slice, and smushes them together
         */
        def combineClumps[B](clumps: Iterable[Set[B]], slice: (B, B)): Iterable[Set[B]] = {
          val (toCombine, toLeave) = clumps.partition(c => c.contains(slice._1) || c.contains(slice._2))
          toLeave ++ List(toCombine.flatten.toSet)
        }
        val initialClumps = for (o <- objects; x <- sets(o)) yield Set((o, x))
        val arrows = for (s <- objects; x <- sets(s); t <- objects; y <- functions(s)(t)(x)) yield ((s, x), (t, y))

        val resultClumps = arrows.foldLeft(initialClumps)(combineClumps _)
        def resultFunctions(o: fgCategory.O)(a: A) = resultClumps.find(_.contains((o, a))).get

        (resultClumps, resultFunctions _)
      }

      val (clumps, functions) = concreteColimit[Any](
        objects,
        { o: fgCategory.O => functorToSet(o).toIterable },
        { s: fgCategory.O => { t: fgCategory.O => { a: Any => (for (g <- generators(s, t)) yield functorToSet(g).toFunction(a)).toSet } } })

      val resultSet = new FSet {
        override def sizeIfFinite = Some(clumps.size)
        override def toIterable = clumps
      }

      new InitialObject[functorToSet.CoCone, functorToSet.CoConeMap] {
        val initialObject = new functorToSet.CoCone {
          override val terminalSet = resultSet
          override def mapToTerminalSet(o: fgCategory.O) = new coConeFunction(o) {
            override def toFunction = functions(o)
          }
        }
        def morphismTo(other: functorToSet.CoCone) = {
          new functorToSet.CoConeMap {
            override val source = initialObject
            override val target = other
            override val terminalMap = new terminalFunction {
              override def toFunction = { x: Set[(fgCategory.O, Any)] => { val h = x.head; other.mapToTerminalSet(h._1).toFunction(h._2) }}.asInstanceOf[Any => Any]
            }
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
    def internalize(t: net.metaphor.api.NaturalTransformationToSet): T = ??? //???
  }
}

