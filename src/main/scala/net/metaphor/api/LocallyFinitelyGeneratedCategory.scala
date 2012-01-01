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
        ???
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

  def pathEquality(path1: Path, path2: Path): Boolean

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
  
  private trait Truncation extends Wrapper with FinitelyGeneratedCategory {
    override def objectsAtLevel(k: Int) = {
      if(k <= maximumLevel) {
        lfgCategory.objectsAtLevel(k)
      } else {
        Nil
      }
    }
  }
  
  private class ConcreteTruncation(override val maximumLevel: Int) extends Truncation with FinitelyGeneratedCategories.StandardFunctorsToSet
  
  def truncateAtLevel(maximumLevel: Int): FinitelyGeneratedCategory = new ConcreteTruncation(maximumLevel)
}