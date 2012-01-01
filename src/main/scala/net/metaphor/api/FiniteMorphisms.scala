package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

trait NormalForm { fpCategory: FinitelyPresentedCategory =>
  def normalForm(p: Path): Path

  def normalWordsOfLength(k: Int)(source: fpCategory.O, target: fpCategory.O): List[Path] = {
    wordsOfLength(k)(source, target).filter(inNormalForm _)
  }

  def inNormalForm(p: Path) = p == normalForm(p)
  override def pathEquality(p1: fpCategory.Path, p2: fpCategory.Path) = {
    normalForm(p1) == normalForm(p2)
  }
}

trait FiniteMorphisms extends NormalForm { fpCategory: FinitelyPresentedCategory =>
  // TODO How would instances arise, which aren't acyclic?
  // We'd need a proof there are only finitely many loops/relations

  def maximumWordLength(source: fpCategory.O, target: fpCategory.O): Int
  def maximumWordLength: Int = (for (s <- objects; t <- objects) yield maximumWordLength(s, t)).max
  def normalWords(source: fpCategory.O, target: fpCategory.O) = (for (k <- 0 to maximumWordLength(source, target); w <- normalWordsOfLength(k)(source, target)) yield w).toList

  def normalForm(p: Path): Path
}

trait FiniteByExhaustion extends FiniteMorphisms { category: FinitelyPresentedCategory =>
  // returns all paths which can be obtained by applying one relation
  private def adjacentPaths(p: Path): Set[Path] = {
    (for (
      i <- 0 until p.length;
      j <- i + 1 until p.length;
      slice = p.morphisms.slice(i, j);
      s = generatorSource(slice.head);
      t = generatorTarget(slice.last);
      subpath = Path(s, t, slice);
      (r1, r2) <- relations(s, t) ::: relations(s, t).map(_.swap);
      if (r1 == subpath)
    ) yield {
      Path(p.source, p.target, p.morphisms.take(i) ::: r2.morphisms ::: p.morphisms.drop(j))
    }).toSet
  }

  private val cachedEquivalenceClasses = net.tqft.toolkit.functions.Memo({ (s: O, t: O) => allEquivalenceClasses._2.filter(c => c.head.source == s && c.head.target == t) })

  private val allEquivalenceClasses: (Int, Set[Set[Path]]) = {
    def equivalenceClassesUpToLength(k: Int): Set[Set[Path]] = {
      def combineClumps[B](clumps: Set[Set[B]], clump: Set[B]): Set[Set[B]] = {
        val (toCombine, toLeave) = clumps.partition(c => c.intersect(clump).nonEmpty)
        toLeave ++ Set(toCombine.flatten.toSet)
      }
      val words = allWordsUpToLength(k).toSet
      val result = words.map(p => adjacentPaths(p) + p).foldLeft(words.map(Set(_)))(combineClumps _)
      println(result)
      if (k > 10) throw new NullPointerException
      result
    }

    def checkLongPathsShorten(k: Int, equivalenceClasses: Set[Set[Path]]): Boolean = {
      (for (e <- equivalenceClasses; if e.exists(_.length == k); if !e.exists(_.length < k)) yield e).isEmpty
    }

    NonStrictNaturalNumbers.map(n => (n, equivalenceClassesUpToLength(n))).find({ case (n, ec) => checkLongPathsShorten(n, ec) }).get
  }

  // a collection of sets of equivalent paths, such that for k = maximumWordLength(s, t) + 1, every path of length <= k appears in some set,
  // and every path of length exactly k appears in a set also containing a shorter element.
  private def pathEquivalenceClasses(s: O, t: O): Set[Set[Path]] = cachedEquivalenceClasses(s, t)

  // maximumWordLength is actually just constant, in the current implementation
  override def maximumWordLength(s: O, t: O) = allEquivalenceClasses._1 - 1

  override def normalForm(m: Path): Path = {
    if (m.length > 10) throw new IllegalArgumentException
    if (m.length <= maximumWordLength(m.source, m.target) + 1) {
      pathEquivalenceClasses(m.source, m.target).find(_.contains(m)).get.toList.sortBy(_.length).head
    } else {
      val s = m.subpath(0, m.length - 1)
      val ns = normalForm(s)
      require(ns.length < s.length)
      ns andThen m.subpath(m.length - 1, m.length)
    }
  }

}

trait Acyclic extends FiniteMorphisms { fpCategory: FinitelyPresentedCategory =>
  private def verifyAcyclicity: Boolean = {
    for (w <- allNontrivialWords) {
      if (w.source == w.target) return false
    }
    return true
  }

  private def computeMaximumWordLength(s: O, t: O) = {
    import net.tqft.toolkit.arithmetic.MinMax._
    words(s, t).map(_.length).maxOption.getOrElse(0)
  }
  private val cachedMaximumWordLength = net.tqft.toolkit.functions.Memo(computeMaximumWordLength _)
  override def maximumWordLength(s: O, t: O) = cachedMaximumWordLength(s, t)
  
  require(verifyAcyclicity)
}

trait Free { fpCategory: FinitelyPresentedCategory =>
  require(allRelations.isEmpty)
  
  override def pathEquality(p1: Path, p2: Path) = p1 == p2
}

trait FreeAcyclic extends Free with Acyclic { fpCategory: FinitelyPresentedCategory => 
  override def normalForm(p: Path) = p
}
