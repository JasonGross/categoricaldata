package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

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
      val (source, target) = if (morphisms.isEmpty) {
        if(i == 0) {
          val s = path.source
          (s, s)
        } else {
          val t = generatorTarget(path.morphisms(i - 1))
          (t, t)
        }
      } else {
        (generatorSource(morphisms.head), generatorTarget(morphisms.last))
      }
      Path(source, target, morphisms)
    }
  }

  val minimumLevel: Int

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
  def generatorsFrom(source: O): List[G]
  def generatorsTo(target: O): List[G]

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
        case other: LocallyFinitelyGeneratedCategory#PathEquivalenceClass =>  pathEquality(representative, other.representative.asInstanceOf[Path])
        case _ => false
      }
    }
    override def hashCode = pathHashCode(representative)
    override def toString = representative.toString
  }

  def pathEquality(path1: Path, path2: Path): Boolean
  /**
   * The only constraint on pathHashCode is the equal paths must give the same result.
   * The extent possible, unequal paths should give different results, as long as this is a cheap calculation.
   * Categories with normal forms override this automatically.
   */
  def pathHashCode(path: Path): Int = 0

  def wordsOfLengthFrom(k: Int)(source: O): List[Path] = {
    k match {
      case 0 => {
        List(Path(source, source, Nil))
      }
      case 1 => generatorsFrom(source).map(generatorAsPath _)
      case _ => for (Path(_, target, morphisms) <- wordsOfLengthFrom(k - 1)(source); g <- generatorsFrom(target)) yield Path(source, generatorTarget(g), morphisms ::: List(g))
    }
  }
  def wordsOfLength(k: Int)(source: O, target: O): List[Path] = wordsOfLengthFrom(k)(source).filter(_.target == target)
  def wordsFrom(source: O) = (for (k <- NonStrictNaturalNumbers) yield wordsOfLengthFrom(k)(source)).takeWhile(_.nonEmpty).flatten
  def words(source: O, target: O) = wordsFrom(source).filter(_.target == target)

  def wordsUpToLength(k: Int)(source: O, target: O): List[Path] = for (n <- (0 to k).toList; w <- wordsOfLength(n)(source, target)) yield w

  // TODO this is very inefficient, we probably should memo some results.
  def morphismsOfLength(k: Int)(source: O, target: O): Set[M] = {
   wordsUpToLength(k)(source, target).map(pathAsMorphism(_)).toSet -- wordsUpToLength(k-1)(source, target).map(pathAsMorphism(_)).toSet 
  }
  
  trait OppositeLocallyFinitelyGeneratedCategory extends LocallyFinitelyGeneratedCategory {
    override type O = lfgCategory.O

    def reverseGenerator(g: lfgCategory.G): G
    def unreverseGenerator(g: G): lfgCategory.G

    def reverse(m: lfgCategory.M): M = m match {
      case lfgCategory.PathEquivalenceClass(Path(source, target, generators)) => PathEquivalenceClass(Path(target, source, generators.reverse.map(reverseGenerator(_))))
    }
    def unreverse(m: M): lfgCategory.M = m match {
      case PathEquivalenceClass(Path(source, target, generators)) => lfgCategory.PathEquivalenceClass(Path(target, source, generators.reverse.map(unreverseGenerator(_))))
    }

    // reverse all the levels!
    override def objectsAtLevel(k: Int) = lfgCategory.objectsAtLevel(-k)
    override def generators(source: O, target: O) = lfgCategory.generators(target, source).map(reverseGenerator(_))

    override def generatorSource(g: G) = lfgCategory.generatorTarget(unreverseGenerator(g))
    override def generatorTarget(g: G) = lfgCategory.generatorSource(unreverseGenerator(g))

    override def pathEquality(p1: Path, p2: Path) = lfgCategory.pathEquality(unreverse(p1).representative, unreverse(p2).representative)
  }

  protected trait Wrapper extends LocallyFinitelyGeneratedCategory {
    override type O = lfgCategory.O
    override type G = lfgCategory.G

    override val minimumLevel = lfgCategory.minimumLevel

    override def objectsAtLevel(k: Int) = lfgCategory.objectsAtLevel(k)
    override def generators(s: O, t: O) = lfgCategory.generators(s, t)
    override def generatorSource(g: G) = lfgCategory.generatorSource(g)
    override def generatorTarget(g: G) = lfgCategory.generatorTarget(g)

    override def pathEquality(p1: Path, p2: Path) = lfgCategory.pathEquality(p1, p2)
  }

  trait Truncation extends Wrapper with FinitelyGeneratedCategory {
    override def objectsAtLevel(k: Int) = {
      if (k <= maximumLevel) {
        lfgCategory.objectsAtLevel(k)
      } else {
        Nil
      }
    }
  }

  private class ConcreteTruncation(override val maximumLevel: Int) extends Truncation with FinitelyGeneratedCategories.StandardFunctorsToSet

  class TruncationFunctor(maximumLevel: Int) extends FunctorWithFinitelyGeneratedSource  {
    override val source: Truncation = new ConcreteTruncation(maximumLevel)
    override val target: lfgCategory.type = lfgCategory
    override def onObjects(o: source.O) = o
    override def onGenerators(g: source.G) = g
  }
  
  def truncationFunctorAtLevel(maximumLevel: Int): TruncationFunctor = new TruncationFunctor(maximumLevel)
  def truncateAtLevel(maximumLevel: Int): FinitelyGeneratedCategory = truncationFunctorAtLevel(maximumLevel).source
  
  trait FunctorToSet extends super.FunctorToSet { functorToSet =>
    def onGenerators(g: G): FFunction
    override def onMorphisms(m: M) = {
      val start = onObjects(source.source(m))
      val morphisms = for (g <- m.representative.morphisms) yield onGenerators(g)
      target.compose(start, morphisms)
    }

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
    trait Cones extends Category {
      override type O = Cone
      override type M = ConeMap
      
      override def identity(c: Cone) = ???
      override def source(c: ConeMap) = c.source
      override def target(c: ConeMap) = c.target
      override def compose(c1: ConeMap, c2: ConeMap) = new ConeMap {
        override val source = c1.source
        override val target = c2.target
        override val initialMap = new initialFunction {
          override def toFunction = c1.initialMap.toFunction andThen c2.initialMap.toFunction
        }
      }
    }
    trait CoCones extends Category {
      override type O = CoCone
      override type M = CoConeMap
      
      override def identity(c: CoCone) = ???
      override def source(c: CoConeMap) = c.source
      override def target(c: CoConeMap) = c.target
      override def compose(c1: CoConeMap, c2: CoConeMap) = new CoConeMap {
        override val source = c1.source
        override val target = c2.target
        override val terminalMap = new terminalFunction {
          override def toFunction = c1.terminalMap.toFunction andThen c2.terminalMap.toFunction
        }
      }
    }
    
    
    def limitApproximation(n: Int) = truncationFunctorAtLevel(n).pullback(functorToSet).limit
    def colimitApproximation(n: Int) = truncationFunctorAtLevel(n).pullback(functorToSet).colimit
  }
}