package net.metaphor.api

trait Functor {
  val source: Category
  val target: Category

  final def apply(o: source.O): target.O = onObjects(o)
  // the dummy implicit argument is a hack to allow overloading of apply
  final def apply(m: source.M)(implicit d: DummyImplicit): target.M = onMorphisms(m)

  def onObjects(o: source.O): target.O
  def onMorphisms(m: source.M): target.M
}

object Functor {
  class IdentityFunctor(val category: Category) extends Functor {
    val source: category.type = category
    val target: category.type = category
    def onObjects(o: category.O) = o
    def onMorphisms(m: category.M) = m
  }

  class CompositeFunctor(val functor1: Functor, val functor2: Functor) extends Functor {
    require(functor1.target == functor2.source)
    val source: functor1.source.type = functor1.source
    val target: functor2.target.type = functor2.target
    def onObjects(o: source.O) = functor2(functor1(o).asInstanceOf[functor2.source.O])
    def onMorphisms(m: source.M) = functor2(functor1(m).asInstanceOf[functor2.source.M])
  }

  trait withSmallSource extends Functor {
    override val source: SmallCategory
  }
  trait withSmallTarget extends Functor {
    override val target: SmallCategory
  }

  trait withLocallyFinitelyGeneratedSource extends withSmallSource { functor =>
    override val source: LocallyFinitelyGeneratedCategory
    def onGenerators(g: source.G): target.M
    final override def onMorphisms(m: source.M) = {
      val start = onObjects(source.source(m))
      val morphisms = for (g <- m.representative.morphisms) yield onGenerators(g)
      target.compose(start, morphisms)
    }

    /**
     * This is a little confusing. SliceCategory is abstract, but always has to be a FinitelyGeneratedCategory.
     * Lower down the hierarchy, the remaining methods can be filled in, either as a truncated SliceCategory, or the honest thing if that's really finite generated.
     *
     */
    abstract class SliceCategory(onLeft: functor.target.O) extends FinitelyGeneratedCategory with FinitelyGeneratedCategories.StandardFunctorsToSet { sliceCategory =>
      override type O = ObjectRightOf
      override type G = ObjectRightOfMap

      override def generatorSource(g: G) = g.source
      override def generatorTarget(g: G) = g.target

      case class ObjectRightOf(right: functor.source.O, morphism: functor.target.M) {
        override def toString = ("(" + morphism.toString + " = F(" + right.toString + "))").replace('"', ''') // this replacement is a hack, so double quotes never show up in JSON
        // we need to provide our own equals method, to ignore the outer class
        override def equals(other: Any) = {
          other match {
            case other: SliceCategory#ObjectRightOf => {
              right == other.right && morphism == other.morphism
            }
            case _ => false
          }
        }
        override def hashCode = (right, morphism).hashCode
        require(functor.target.target(morphism) == functor.apply(right))
        require(functor.target.source(morphism) == onLeft)
      }
      case class ObjectRightOfMap(source: ObjectRightOf, target: ObjectRightOf, generator: functor.source.G) {
        // we need to provide our own equals method, to ignore the outer class
        override def equals(other: Any) = {
          other match {
            case other: SliceCategory#ObjectRightOfMap => {
              source == other.source && target == other.target && generator == other.generator
            }
            case _ => false
          }
        }
        override def hashCode = (source, target, generator).hashCode
        require(functor.source.generatorSource(generator) == source.right)
        require(functor.source.generatorTarget(generator) == target.right)
        require(functor.target.compose(source.morphism, functor.onGenerators(generator)) == target.morphism)
      }

      override val minimumLevel: Int = functor.source.minimumLevel

      override def generators(source: ObjectRightOf, target: ObjectRightOf): List[ObjectRightOfMap] = {
        import functor.source.generatorAsMorphism
        for (g <- functor.source.generators(source.right, target.right); if functor.target.compose(source.morphism, functor.apply(g)) == target.morphism) yield {
          ObjectRightOfMap(source, target, g)
        }
      }

      override def pathEquality(p1: Path, p2: Path) = {
        val x1 = Path(p1.source.right, p1.target.right, p1.morphisms.map(_.generator))
        val x2 = Path(p2.source.right, p2.target.right, p2.morphisms.map(_.generator))
        functor.source.pathEquality(x1, x2)
      }
    }

    abstract class CosliceCategory(onRight: functor.target.O) extends FinitelyGeneratedCategory with FinitelyGeneratedCategories.StandardFunctorsToSet { cosliceCategory =>
      override type O = ObjectLeftOf
      override type G = ObjectLeftOfMap

      override def generatorSource(g: G) = g.source
      override def generatorTarget(g: G) = g.target

      case class ObjectLeftOf(left: functor.source.O, morphism: functor.target.M) {
        override def toString = ("(F(" + left.toString + ") = " + morphism.toString + ")").replace('"', ''') // this replacement is a hack, so double quotes never show up in JSON
        // we need to provide our own equals method, to ignore the outer class
        override def equals(other: Any) = {
          other match {
            case other: CosliceCategory#ObjectLeftOf => {
              left == other.left && morphism == other.morphism
            }
            case _ => false
          }
        }
        override def hashCode = (left, morphism).hashCode
        require(functor.target.source(morphism) == functor.apply(left))
        require(functor.target.target(morphism) == onRight)
      }
      case class ObjectLeftOfMap(source: ObjectLeftOf, target: ObjectLeftOf, generator: functor.source.G) {
        // we need to provide our own equals method, to ignore the outer class
        override def equals(other: Any) = {
          other match {
            case other: CosliceCategory#ObjectLeftOfMap => {
              source == other.source && target == other.target && generator == other.generator
            }
            case _ => false
          }
        }
        override def hashCode = (source, target, generator).hashCode
        require(functor.source.generatorSource(generator) == source.left)
        require(functor.source.generatorTarget(generator) == target.left)
        require(functor.target.compose(functor.onGenerators(generator), target.morphism) == source.morphism)
      }

      override val minimumLevel: Int = functor.source.minimumLevel

      override def generators(source: ObjectLeftOf, target: ObjectLeftOf): List[ObjectLeftOfMap] = {
        import functor.source.generatorAsMorphism
        for (g <- functor.source.generators(source.left, target.left); if functor.target.compose(functor.apply(g), target.morphism) == source.morphism) yield {
          ObjectLeftOfMap(source, target, g)
        }
      }

      override def pathEquality(p1: Path, p2: Path) = {
        val x1 = Path(p1.source.left, p1.target.left, p1.morphisms.map(_.generator))
        val x2 = Path(p2.source.left, p2.target.left, p2.morphisms.map(_.generator))
        functor.source.pathEquality(x1, x2)
      }
    }

  }
  trait withLocallyFinitelyGeneratedTarget extends withSmallTarget {
    override val target: LocallyFinitelyGeneratedCategory
  }

  trait withFinitelyGeneratedSource extends withLocallyFinitelyGeneratedSource {
    override val source: FinitelyGeneratedCategory
  }
  trait withFinitelyGeneratedTarget extends withLocallyFinitelyGeneratedTarget {
    override val target: FinitelyGeneratedCategory
  }

  trait withFinitelyPresentedSource extends withFinitelyGeneratedSource { functor =>
    override val source: FinitelyPresentedCategory
    def verifyRelations = {
      for (relation <- source.allRelations) {
        require(functor.onMorphisms(source.pathAsMorphism(relation._1)) == functor.onMorphisms(source.pathAsMorphism(relation._2)))
      }
    }
  }
  trait withFinitelyPresentedTarget extends withFinitelyGeneratedTarget {
    override val target: FinitelyPresentedCategory
  }

  object withSmallSource {
    trait withSmallTarget extends Functor.withSmallSource with Functor.withSmallTarget { smallFunctor =>
      trait ContravariantDataFunctor extends Functor {
        override val source = smallFunctor.target.AllFunctorsToSet
        override val target: smallFunctor.source.SpecializedFunctorsToSet = smallFunctor.source.functorsToSet
        def andThen(g: CovariantDataFunctor) = DataFunctors.compose(this, g)
      }
      trait CovariantDataFunctor extends Functor {
        override val source = smallFunctor.source.AllFunctorsToSet
        override val target: smallFunctor.target.SpecializedFunctorsToSet = smallFunctor.target.functorsToSet
        def andThen(g: ContravariantDataFunctor) = DataFunctors.compose(this, g)
      }

      object DataFunctors {
        class TargetComposition(f: ContravariantDataFunctor, g: CovariantDataFunctor) extends Functor {
          override val source: f.source.type = f.source
          override val target: g.target.type = g.target
          override def onObjects(o: source.O): target.O = g(f(o))
          override def onMorphisms(m: source.M): target.M = g(f(m))
        }
        class SourceComposition(f: CovariantDataFunctor, g: ContravariantDataFunctor) extends Functor {
          override val source: f.source.type = f.source
          override val target: g.target.type = g.target
          override def onObjects(o: source.O): target.O = g(f(o))
          override def onMorphisms(m: source.M): target.M = g(f(m))
        }

        def compose(f: ContravariantDataFunctor, g: CovariantDataFunctor) = new TargetComposition(f, g)
        def compose(f: CovariantDataFunctor, g: ContravariantDataFunctor) = new SourceComposition(f, g)
      }

      trait Pullback extends ContravariantDataFunctor {
        override def onObjects(i: smallFunctor.target.FunctorToSet) = smallFunctor.source.internalize(new smallFunctor.source.FunctorToSet {
          def onObjects(o: smallFunctor.source.O) = i(smallFunctor.apply(o))
          def onMorphisms(m: smallFunctor.source.M) = i(smallFunctor.apply(m))
        })
        override def onMorphisms(m: smallFunctor.target.NaturalTransformationToSet) = smallFunctor.source.internalize(new smallFunctor.source.NaturalTransformationToSet {
          val source = onObjects(m.target)
          val target = onObjects(m.source)
          def apply(o: smallFunctor.source.O) = m(smallFunctor.apply(o))
        })

        // c.f. Functor.withFinitelyGeneratedSource.withFinitelyGeneratedTarget.Pullback, which adds limitMorphism
      }

      lazy val pullback = new Pullback {}

      lazy val ^* = new Functor {
        override val source = FunctorsToSet
        override val target = smallFunctor.source.functorsToSet
        def onObjects(i: FunctorToSet) = pullback.apply(smallFunctor.target.internalize(i))
        def onMorphisms(t: NaturalTransformationToSet) = pullback.apply(smallFunctor.target.internalize(t))
      }

    }
    trait withLocallyFinitelyGeneratedTarget extends Functor.withSmallSource.withSmallTarget with Functor.withLocallyFinitelyGeneratedTarget
    trait withFinitelyGeneratedTarget extends Functor.withSmallSource.withLocallyFinitelyGeneratedTarget with Functor.withFinitelyGeneratedTarget
    trait withFinitelyPresentedTarget extends Functor.withSmallSource.withFinitelyGeneratedTarget with Functor.withFinitelyPresentedTarget
  }
  object withLocallyFinitelyGeneratedSource {
    trait withSmallTarget extends Functor.withSmallSource.withSmallTarget with Functor.withLocallyFinitelyGeneratedSource
    trait withLocallyFinitelyGeneratedTarget extends Functor.withSmallSource.withLocallyFinitelyGeneratedTarget with Functor.withLocallyFinitelyGeneratedSource.withSmallTarget { lfgFunctor =>
      abstract class SliceCategory(onLeft: lfgFunctor.target.O) extends super.SliceCategory(onLeft) {
        override def objectsAtLevel(k: Int): List[ObjectRightOf] = {
          for (
            l <- (lfgFunctor.source.minimumLevel to k).toList;
            right <- lfgFunctor.source.objectsAtLevel(l);
            path <- lfgFunctor.target.wordsOfLength(k - l)(onLeft, lfgFunctor.apply(right))
          ) yield ObjectRightOf(right, lfgFunctor.target.pathAsMorphism(path))
        }
      }
      abstract class CosliceCategory(onRight: lfgFunctor.target.O) extends super.CosliceCategory(onRight) {
        override def objectsAtLevel(k: Int): List[ObjectLeftOf] = {
          for (
            l <- (lfgFunctor.source.minimumLevel to k).toList;
            left <- lfgFunctor.source.objectsAtLevel(l);
            path <- lfgFunctor.target.wordsOfLength(k - l)(lfgFunctor.apply(left), onRight)
          ) yield ObjectLeftOf(left, lfgFunctor.target.pathAsMorphism(path))
        }
      }

    }
    trait withFinitelyGeneratedTarget extends Functor.withSmallSource.withFinitelyGeneratedTarget with Functor.withLocallyFinitelyGeneratedSource.withLocallyFinitelyGeneratedTarget
    trait withFinitelyPresentedTarget extends Functor.withSmallSource.withFinitelyPresentedTarget with Functor.withLocallyFinitelyGeneratedSource.withFinitelyGeneratedTarget
  }
  object withFinitelyGeneratedSource {
    trait withSmallTarget extends Functor.withLocallyFinitelyGeneratedSource.withSmallTarget with Functor.withFinitelyGeneratedSource
    trait withLocallyFinitelyGeneratedTarget extends Functor.withLocallyFinitelyGeneratedSource.withLocallyFinitelyGeneratedTarget with Functor.withFinitelyGeneratedSource.withSmallTarget { functor =>
      class SliceCategory(maximumPathLength: Int, onLeft: functor.target.O) extends super.SliceCategory(onLeft) {
        override val maximumLevel: Int = functor.source.maximumLevel + maximumPathLength

        override def internalize(f: net.metaphor.api.FunctorToSet) = new FunctorToSet {
          require(f.source == source)
          override def onObjects(o: source.O) = f.onObjects(o.asInstanceOf[f.source.O])
          override def onGenerators(g: source.G) = f.onMorphisms(generatorAsMorphism(g).asInstanceOf[f.source.M])
        }
      }
      class CosliceCategory(maximumPathLength: Int, onRight: functor.target.O) extends super.CosliceCategory(onRight) {
        override val maximumLevel: Int = functor.source.maximumLevel + maximumPathLength

        override def internalize(f: net.metaphor.api.FunctorToSet) = new FunctorToSet {
          require(f.source == source)
          override def onObjects(o: source.O) = f.onObjects(o.asInstanceOf[f.source.O])
          override def onGenerators(g: source.G) = f.onMorphisms(generatorAsMorphism(g).asInstanceOf[f.source.M])
        }
      }
    }
    trait withFinitelyGeneratedTarget extends Functor.withLocallyFinitelyGeneratedSource.withFinitelyGeneratedTarget with Functor.withFinitelyGeneratedSource.withLocallyFinitelyGeneratedTarget { fgFunctor =>
      abstract class SliceFunctor extends Functor { sliceFunctor => // D^op --> Cat_{/C}
        override val source: fgFunctor.target.opposite.type = fgFunctor.target.opposite
        override val target: fgFunctor.source.FinitelyGeneratedCategoriesOver = fgFunctor.source.finitelyGeneratedCategoriesOver
        override def onObjects(s: source.O): SliceCategoryOver = new SliceCategoryOver(s)
        override def onMorphisms(m: source.M): SliceFunctorOver = new SliceFunctorOver(m)

        class SliceCategoryOver(onLeft: fgFunctor.target.opposite.O) extends fgFunctor.source.FinitelyGeneratedCategoryOver { // (d | F) --> C
          override val source = buildSliceCategory(onLeft)
          override def onObjects(o: source.O) = o.right
          override def onGenerators(g: source.G) = {
            import fgFunctor.source.generatorAsMorphism
            g.generator
          }
        }
        class SliceFunctorOver(m: fgFunctor.target.opposite.M) extends fgFunctor.source.FinitelyGeneratedFunctorOver { // d --> d' ~> (d' | F) --> (d | F)
          override val source = sliceFunctor.onObjects(fgFunctor.target.opposite.source(m))
          override val target = sliceFunctor.onObjects(fgFunctor.target.opposite.target(m))
          override val functor = new F {
            override def onObjects(o: source.O): target.ObjectRightOf = {
              target.ObjectRightOf(right = o.right, morphism = fgFunctor.target.compose(fgFunctor.target.opposite.unreverse(m), o.morphism))
            }
            override def onGenerators(g: source.G): target.M = {
              target.generatorAsMorphism(target.ObjectRightOfMap(onObjects(g.source), onObjects(g.target), g.generator))
            }
          }
        }

        def buildSliceCategory: fgFunctor.target.O => SliceCategory
      }

      abstract class CosliceFunctor extends Functor { cosliceFunctor => // D --> Cat_{/C}
        override val source: fgFunctor.target.type = fgFunctor.target
        override val target: fgFunctor.source.FinitelyGeneratedCategoriesOver = fgFunctor.source.finitelyGeneratedCategoriesOver
        override def onObjects(s: source.O): CosliceCategoryOver = new CosliceCategoryOver(s)
        override def onMorphisms(m: source.M): CosliceFunctorOver = new CosliceFunctorOver(m)

        class CosliceCategoryOver(onRight: fgFunctor.target.O) extends fgFunctor.source.FinitelyGeneratedCategoryOver { // (F | d) --> C
          override val source = buildCosliceCategory(onRight)
          override def onObjects(o: source.O) = o.left
          override def onGenerators(g: source.G) = {
            import fgFunctor.source.generatorAsMorphism
            g.generator
          }
        }

        class CosliceFunctorOver(m: fgFunctor.target.M) extends fgFunctor.source.FinitelyGeneratedFunctorOver { // d --> d' ~> (F | d) --> (F | d')
          override val source = cosliceFunctor.onObjects(fgFunctor.target.source(m))
          override val target = cosliceFunctor.onObjects(fgFunctor.target.target(m))
          override val functor = new F {
            override def onObjects(o: source.O): target.ObjectLeftOf = {
              target.ObjectLeftOf(left = o.left, morphism = fgFunctor.target.compose(o.morphism, m))
            }
            override def onGenerators(g: source.G): target.M = {
              target.generatorAsMorphism(target.ObjectLeftOfMap(onObjects(g.target), onObjects(g.source), g.generator))
            }
          }
        }

        def buildCosliceCategory: fgFunctor.target.opposite.O => CosliceCategory
      }

      trait Pullback extends super.Pullback {
        def limitMorphism(i: fgFunctor.target.FunctorToSet) = FFunction(
          source = onObjects(i).limitSet,
          target = i.limitSet,
          function = { x: Map[fgFunctor.source.O, Any] => ??? /* TODO build something, via the universal property */ })
      }

      override lazy val pullback = new Pullback {}
    }
    trait withFinitelyPresentedTarget extends Functor.withLocallyFinitelyGeneratedSource.withFinitelyPresentedTarget with Functor.withFinitelyGeneratedSource.withFinitelyGeneratedTarget
  }
  object withFinitelyPresentedSource {
    trait withSmallTarget extends Functor.withFinitelyGeneratedSource.withSmallTarget with Functor.withFinitelyPresentedSource
    trait withLocallyFinitelyGeneratedTarget extends Functor.withFinitelyGeneratedSource.withLocallyFinitelyGeneratedTarget with Functor.withFinitelyPresentedSource.withSmallTarget
    trait withFinitelyGeneratedTarget extends Functor.withFinitelyGeneratedSource.withFinitelyGeneratedTarget with Functor.withFinitelyPresentedSource.withLocallyFinitelyGeneratedTarget
    trait withFinitelyPresentedTarget extends Functor.withFinitelyGeneratedSource.withFinitelyPresentedTarget with Functor.withFinitelyPresentedSource.withFinitelyGeneratedTarget
  }
}

trait MemoFunctor extends Functor {
  import net.tqft.toolkit.functions.Memo
  val memoOnObjects = Memo(super.onObjects _)
  val memoOnMorphisms = Memo(super.onMorphisms _)
  abstract override def onObjects(o: source.O) = memoOnObjects(o)
  abstract override def onMorphisms(o: source.M) = memoOnMorphisms(o)
}
