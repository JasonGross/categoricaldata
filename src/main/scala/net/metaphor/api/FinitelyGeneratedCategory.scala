package net.metaphor.api
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.collections.NonStrictNaturalNumbers

/**
 * A FinitelyGeneratedCategory is just a LocallyFinitelyGeneratedCategory with finitely many levels (and so finitely many objects and generators).
 */
trait FinitelyGeneratedCategory extends LocallyFinitelyGeneratedCategory { fgCategory =>
  val maximumLevel: Int

  lazy val objects: List[O] = for (k <- (minimumLevel to maximumLevel).toList; o <- objectsAtLevel(k)) yield o
  override def objectSet: FSet = new FSet {
    def toIterable = objects
    lazy val sizeIfFinite = Some(toIterable.size)
  }

  def generatorsFrom(source: O) = for (target <- objects; g <- generators(source, target)) yield g
  def generatorsTo(target: O) = for (source <- objects; g <- generators(source, target)) yield g
  def allGenerators: List[G] = for (source <- objects; target <- objects; g <- generators(source, target)) yield g

  def allWordsOfLength(k: Int): List[Path] = {
    for (s <- objects; t <- objects; w <- wordsOfLength(k)(s, t)) yield w
  }
  def allWordsUpToLength(k: Int): List[Path] = for (n <- (0 to k).toList; w <- allWordsOfLength(n)) yield w
  
  def allWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k)).takeWhile(_.nonEmpty).flatten
  def allNontrivialWords = (for (k <- NonStrictNaturalNumbers) yield allWordsOfLength(k + 1)).takeWhile(_.nonEmpty).flatten

  trait OppositeFinitelyGeneratedCategory extends FinitelyGeneratedCategory with OppositeLocallyFinitelyGeneratedCategory { opposite =>
    override val minimumLevel = -fgCategory.maximumLevel
    override val maximumLevel = -fgCategory.minimumLevel
  }

  class ConcreteOpposite extends OppositeFinitelyGeneratedCategory with FinitelyGeneratedCategories.StandardFunctorsToSet {
    override type G = OppositeGenerator
    case class OppositeGenerator(g: fgCategory.G)
    override def reverseGenerator(g: fgCategory.G) = OppositeGenerator(g)
    override def unreverseGenerator(g: OppositeGenerator) = g.g
  }

  override lazy val opposite: OppositeFinitelyGeneratedCategory = new ConcreteOpposite

  trait FinitelyGeneratedCategoryOver extends CategoryOver with FinitelyGeneratedFunctor { categoryOver =>
    override val source: FinitelyGeneratedCategory
    def onGenerators(g: source.G): target.M
    override def onMorphisms(m: source.M) = {
      val start = onObjects(source.source(m))
      val morphisms = for (g <- m.representative.morphisms) yield onGenerators(g)
      target.compose(start, morphisms)
    }
    trait Identity extends FinitelyGeneratedFunctorOver {
      override val source: categoryOver.type = categoryOver
      override val target: categoryOver.type = categoryOver
      override val functor = new F {
        override def onObjects(o: categoryOver.source.O) = o
        override def onGenerators(g: categoryOver.source.G) = categoryOver.source.generatorAsMorphism(g)
      }
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
    override def identity(f: O) = new f.Identity {}
    override def source(t: M) = t.source
    override def target(t: M) = t.target
    override def compose(m1: M, m2: M): M = new FinitelyGeneratedFunctorOver {
      val source = m1.source
      val target = m2.target
      val functor = ??? // it's unclear to me that this is even possible to implement (c.f. CategoriesOver in SmallCategory)
    }
  }

  def finitelyGeneratedCategoriesOver: FinitelyGeneratedCategoriesOver = new FinitelyGeneratedCategoriesOver {}

  override type F <: FunctorToSet
  override type T <: NaturalTransformationToSet

  trait FunctorToSet extends super.FunctorToSet { functorToSet =>
    lazy val totalSet = new FiniteFSet {
      def toIterable = for (o <- objectSet.toIterable; o2 = o.asInstanceOf[O]; x <- functorToSet(o2).toIterable) yield (o, x)
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

    lazy val limit: Cones with TerminalObject = {
      // this is where all the work happens.
      def concreteLimit[A](objects: Iterable[fgCategory.O], sets: fgCategory.O => Iterable[A], functions: fgCategory.O => (fgCategory.O => (A => Set[A]))): (Iterable[fgCategory.O => A], fgCategory.O => ((fgCategory.O => A) => A)) = {
        /**
         *  @returns sets(o2) if there are actually no morphisms from o1 to o2
         * 			 None if there are several morphisms, with different images on a
         * 			 Some(b) if all morphisms send a to b.
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
        override def sizeIfFinite = Some(maps.size)
        override def toIterable = maps.map(new Section(_))
      }

      new Cones with TerminalObject {
        val terminalObject = new functorToSet.Cone {
          override val initialSet = resultSet
          override def mapFromInitialSet(o: fgCategory.O) = new coneFunction(o) {
            override def toFunction = ({ s: Section => s.toFunction.asInstanceOf[fgCategory.O => Any] } andThen functions(o)).asInstanceOf[Any => Any]
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

    lazy val colimit: CoCones with InitialObject = {

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

      new CoCones with InitialObject {
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
              override def toFunction = { x: Set[(fgCategory.O, Any)] => { val h = x.head; other.mapToTerminalSet(h._1).toFunction(h._2) } }.asInstanceOf[Any => Any]
            }
          }
        }
      }
    }

  }

  protected trait Wrapper extends super.Wrapper with FinitelyGeneratedCategory {
    override val maximumLevel = fgCategory.maximumLevel
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
    def internalize(t: net.metaphor.api.NaturalTransformationToSet): T = new NaturalTransformationToSet {
      require(t.sourceCategory == source)
      val source = internalize(t.source)
      val target = internalize(t.target)
      def apply(o: O) = t(o.asInstanceOf[t.sourceCategory.O])
    }
  }
}

