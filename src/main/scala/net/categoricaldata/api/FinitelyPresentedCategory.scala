package net.categoricaldata.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait FinitelyPresentedCategory extends FinitelyGeneratedCategory { fpCategory =>
  def relations(source: O, target: O): List[(Path, Path)]
  def relationsFrom(source: O) = for (target <- objects; r <- relations(source, target)) yield r
  def relationsTo(target: O) = for (source <- objects; r <- relations(source, target)) yield r
  def allRelations: List[(Path, Path)] = for (source <- objects; target <- objects; r <- relations(source, target)) yield r

  private def symmetrizedRelations = allRelations.map({ r => Set(r, r.swap) })

  override def equals(other: Any) = {
    other match {
      case other: FinitelyPresentedCategory => {
        objects.toSet == other.objects.toSet && allGenerators.toSet == other.allGenerators.toSet && symmetrizedRelations.toSet == other.symmetrizedRelations.toSet
      }
      case _ => false
    }
  }

  override def hashCode = {
    (objects.toSet, allGenerators.toSet, symmetrizedRelations.toSet).hashCode
  }

  def findIsomorphismsTo(other: FinitelyPresentedCategory): Iterable[Functor] = ???
  def isIsomorphicTo(other: FinitelyPresentedCategory) = findIsomorphismsTo(other).nonEmpty

  def findEquivalencesTo(other: FinitelyPresentedCategory): Iterable[Equivalence] = ???
  def isEquivalentTo(other: FinitelyPresentedCategory) = findEquivalencesTo(other).nonEmpty

  trait OppositeFinitelyPresentedCategory extends FinitelyPresentedCategory with OppositeFinitelyGeneratedCategory {
    override def relations(source: O, target: O) = for ((Path(_, _, g1s), Path(_, _, g2s)) <- fpCategory.relations(target, source)) yield {
      (Path(source, target, g1s.reverse.map(reverseGenerator(_))), Path(source, target, g2s.reverse.map(reverseGenerator(_))))
    }
  }

  trait Wrapper extends super.Wrapper with FinitelyPresentedCategory {
    override def relations(s: O, t: O) = fpCategory.relations(s, t)
  }

  abstract class FullSubcategory(spannedBy: List[O]) extends super.FullSubcategory(spannedBy) with FinitelyPresentedCategory {
    override def relations(s: O, t: O) = fpCategory.relations(s, t)
  }

  class ConcreteFullSubcategory(spannedBy: List[O]) extends FullSubcategory(spannedBy) with FinitelyGeneratedCategories.StandardFunctorsToSet

  class FullSubcategoryInclusion(spannedBy: List[O]) extends super.FullSubcategoryInclusion(spannedBy) with Functor.withFinitelyPresentedSource.withFinitelyPresentedTarget {
    override val source: FullSubcategory = new ConcreteFullSubcategory(spannedBy)
  }

  override def fullSubcategoryInclusion(spannedBy: List[O]): FullSubcategoryInclusion = new FullSubcategoryInclusion(spannedBy)
  override def fullSubcategory(spannedBy: List[O]): FullSubcategory = fullSubcategoryInclusion(spannedBy).source

  trait FunctorToSet extends super.FunctorToSet { functorToSet =>
    def verifyRelations {
      for (relation <- fpCategory.allRelations) {
        require(functorToSet.onMorphisms(source.pathAsMorphism(relation._1)) == functorToSet.onMorphisms(source.pathAsMorphism(relation._2)))
      }
    }
  }
}

object FinitelyPresentedCategory {
  trait NormalForm { fpCategory: FinitelyPresentedCategory =>
    def normalForm(p: Path): Path

    override def pathEquality(p1: fpCategory.Path, p2: fpCategory.Path) = {
      normalForm(p1) == normalForm(p2)
    }
    override def pathHashCode(p: fpCategory.Path) = normalForm(p).hashCode
  }

  trait FiniteMorphisms extends NormalForm { fpCategory: FinitelyPresentedCategory =>
    def maximumWordLength(source: fpCategory.O, target: fpCategory.O): Int
    lazy val maximumWordLength: Int = (for (s <- objects; t <- objects) yield maximumWordLength(s, t)).max
  }

  trait FiniteByExhaustion extends FiniteMorphisms { category: FinitelyPresentedCategory =>
    // returns all paths which can be obtained by applying one relation
    def adjacentPaths(p: Path): Set[Path] = {
      (for (
        i <- 0 to p.length;
        j <- i to p.length;
        subpath = p.subpath(i, j);
        s = subpath.source;
        t = subpath.target;
        (r1, r2) <- relations(s, t) ::: relations(s, t).map(_.swap);
        if (r1 == subpath)
      ) yield {
        Path(p.source, p.target, p.morphisms.take(i) ::: r2.morphisms ::: p.morphisms.drop(j))
      }).toSet
    }

    // returns a k, and a collection of sets of equivalence paths, such that every path of length <= k appears in some set
    // and every path of length exactly k appears in a set also containing a shorter element.
    private val allEquivalenceClasses: (Int, Set[Set[Path]]) = {
      def equivalenceClassesUpToLength(k: Int): Set[Set[Path]] = {
        def combineClumps[B](clumps: Set[Set[B]], clump: Set[B]): Set[Set[B]] = {
          val (toCombine, toLeave) = clumps.partition(c => c.intersect(clump).nonEmpty)
          toLeave ++ Set(toCombine.flatten.toSet)
        }
        val words = allWordsUpToLength(k).toSet

        words.map(p => adjacentPaths(p) + p).foldLeft(words.map(Set(_)))(combineClumps _)
      }

      def checkLongPathsShorten(k: Int, equivalenceClasses: Set[Set[Path]]): Boolean = {
        (for (e <- equivalenceClasses; if e.exists(_.length == k); if !e.exists(_.length < k)) yield e).isEmpty
      }

      NonStrictNaturalNumbers.map(n => (n, equivalenceClassesUpToLength(n))).find({ case (n, ec) => checkLongPathsShorten(n, ec) }).get
    }

    val (uniformMaximumWordLength, cachedNormalForms) = allEquivalenceClasses match {
      case (k, equivalenceClasses) => {
        (k - 1,
          (equivalenceClasses.groupBy(c => (c.head.source, c.head.target)).map {
            case ((s, t), classes: Set[Set[Path]]) =>
              {
                (s, t) -> ((for (ec <- classes; choice = ec.toList.sortBy(_.length).head; x <- ec) yield (x -> choice)).toMap)
              }
          }))
      }
    }

    // maximumWordLength is actually just constant, in the current implementation
    override def maximumWordLength(s: O, t: O) = uniformMaximumWordLength

    override def normalForm(m: Path): Path = {
      if (m.length <= maximumWordLength(m.source, m.target) + 1) {
        cachedNormalForms(m.source, m.target)(m)
      } else {
        val s = m.subpath(0, m.length - 1)
        val ns = normalForm(s)
        require(ns.length < s.length)
        normalForm(ns andThen m.subpath(m.length - 1, m.length))
      }
    }

  }

  trait Acyclic extends FiniteMorphisms { fpCategory: FinitelyPresentedCategory =>
    def verifyAcyclic: fpCategory.type = {
      for (w <- allNontrivialWords) {
        require(w.source != w.target)
      }
      this
    }

    private def computeMaximumWordLength(s: O, t: O) = {
      import net.tqft.toolkit.arithmetic.MinMax._
      words(s, t).map(_.length).maxOption.getOrElse(0)
    }
    private val cachedMaximumWordLength = net.tqft.toolkit.functions.Memo(computeMaximumWordLength _)
    override def maximumWordLength(s: O, t: O) = cachedMaximumWordLength(s, t)

  }

  trait Free { fpCategory: FinitelyPresentedCategory =>
    def verifyFree: fpCategory.type = {
      require(allRelations.isEmpty)
      this
    }

    override def pathEquality(p1: Path, p2: Path) = p1 == p2
  }

  trait FreeAcyclic extends Free with Acyclic { fpCategory: FinitelyPresentedCategory =>
    override def normalForm(p: Path) = p
  }

}
