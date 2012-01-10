package net.metaphor.api

trait LocallyFinitelyGeneratedFunctor extends Functor.withLocallyFinitelyGeneratedSource.withSmallTarget { lfgFunctor =>
  override val source: LocallyFinitelyGeneratedCategory

  def onGenerators(g: source.G): target.M
  override def onMorphisms(m: source.M) = {
    val start = onObjects(source.source(m))
    val morphisms = for (g <- m.representative.morphisms) yield onGenerators(g)
    target.compose(start, morphisms)
  }

  /**
   * This is a little confusing. SliceCategory is abstract, but always has to be a FinitelyGeneratedCategory.
   * Lower down the hierarchy, the remaining methods can be filled in, either as a truncated SliceCategory, or the honest thing if that's really finite generated.
   *
   */
  abstract class SliceCategory(onLeft: lfgFunctor.target.O) extends FinitelyGeneratedCategory with FinitelyGeneratedCategories.StandardFunctorsToSet { sliceCategory =>
    override type O = ObjectRightOf
    override type G = ObjectRightOfMap

    override def generatorSource(g: G) = g.source
    override def generatorTarget(g: G) = g.target

    case class ObjectRightOf(right: lfgFunctor.source.O, morphism: lfgFunctor.target.M) {
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
      require(lfgFunctor.target.target(morphism) == lfgFunctor.apply(right))
      require(lfgFunctor.target.source(morphism) == onLeft)
    }
    case class ObjectRightOfMap(source: ObjectRightOf, target: ObjectRightOf, generator: lfgFunctor.source.G) {
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
      require(lfgFunctor.source.generatorSource(generator) == source.right)
      require(lfgFunctor.source.generatorTarget(generator) == target.right)
      require(lfgFunctor.target.compose(source.morphism, lfgFunctor.onGenerators(generator)) == target.morphism)
    }

    override val minimumLevel: Int = lfgFunctor.source.minimumLevel

    override def generators(source: ObjectRightOf, target: ObjectRightOf): List[ObjectRightOfMap] = {
      import lfgFunctor.source.generatorAsMorphism
      for (g <- lfgFunctor.source.generators(source.right, target.right); if lfgFunctor.target.compose(source.morphism, lfgFunctor.apply(g)) == target.morphism) yield {
        ObjectRightOfMap(source, target, g)
      }
    }

    override def pathEquality(p1: Path, p2: Path) = {
      val x1 = Path(p1.source.right, p1.target.right, p1.morphisms.map(_.generator))
      val x2 = Path(p2.source.right, p2.target.right, p2.morphisms.map(_.generator))
      lfgFunctor.source.pathEquality(x1, x2)
    }
  }

  abstract class CosliceCategory(onRight: lfgFunctor.target.O) extends FinitelyGeneratedCategory with FinitelyGeneratedCategories.StandardFunctorsToSet { cosliceCategory =>
    override type O = ObjectLeftOf
    override type G = ObjectLeftOfMap

    override def generatorSource(g: G) = g.source
    override def generatorTarget(g: G) = g.target

    case class ObjectLeftOf(left: lfgFunctor.source.O, morphism: lfgFunctor.target.M) {
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
      require(lfgFunctor.target.source(morphism) == lfgFunctor.apply(left))
      require(lfgFunctor.target.target(morphism) == onRight)
    }
    case class ObjectLeftOfMap(source: ObjectLeftOf, target: ObjectLeftOf, generator: lfgFunctor.source.G) {
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
      require(lfgFunctor.source.generatorSource(generator) == source.left)
      require(lfgFunctor.source.generatorTarget(generator) == target.left)
      require(lfgFunctor.target.compose(lfgFunctor.onGenerators(generator), target.morphism) == source.morphism)
    }

    override val minimumLevel: Int = lfgFunctor.source.minimumLevel

    override def generators(source: ObjectLeftOf, target: ObjectLeftOf): List[ObjectLeftOfMap] = {
      import lfgFunctor.source.generatorAsMorphism
      for (g <- lfgFunctor.source.generators(source.left, target.left); if lfgFunctor.target.compose(lfgFunctor.apply(g), target.morphism) == source.morphism) yield {
        ObjectLeftOfMap(source, target, g)
      }
    }

    override def pathEquality(p1: Path, p2: Path) = {
      val x1 = Path(p1.source.left, p1.target.left, p1.morphisms.map(_.generator))
      val x2 = Path(p2.source.left, p2.target.left, p2.morphisms.map(_.generator))
      lfgFunctor.source.pathEquality(x1, x2)
    }
  }

}

trait LocallyFinitelyGeneratedFunctorWithLocallyFinitelyGeneratedTarget extends LocallyFinitelyGeneratedFunctor { lfgFunctor =>
  override val target: LocallyFinitelyGeneratedCategory

  abstract class SliceCategory(onLeft: lfgFunctor.target.O) extends super.SliceCategory(onLeft) {
    override def objectsAtLevel(k: Int): List[ObjectRightOf] = {
      for (
        l <- (lfgFunctor.source.minimumLevel to k).toList;
        right <- lfgFunctor.source.objectsAtLevel(l);
// the next two lines should be swapped out for the following two, but I want to wait for a test        
        path <- lfgFunctor.target.wordsOfLength(k - l)(onLeft, lfgFunctor.apply(right))
      ) yield ObjectRightOf(right, lfgFunctor.target.pathAsMorphism(path))
//        morphism <- lfgFunctor.target.morphismsOfLength(k - l)(onLeft, lfgFunctor.apply(right))
//      ) yield ObjectRightOf(right, morphism)
    }
  }
  abstract class CosliceCategory(onRight: lfgFunctor.target.O) extends super.CosliceCategory(onRight) {
    override def objectsAtLevel(k: Int): List[ObjectLeftOf] = {
      for (
        l <- (lfgFunctor.source.minimumLevel to k).toList;
        left <- lfgFunctor.source.objectsAtLevel(l);
// the next two lines should be swapped out for the following two       
        path <- lfgFunctor.target.wordsOfLength(k - l)(lfgFunctor.apply(left), onRight)
      ) yield ObjectLeftOf(left, lfgFunctor.target.pathAsMorphism(path))
//        morphism <- lfgFunctor.target.morphismsOfLength(k - l)(lfgFunctor.apply(left), onRight)
//      ) yield ObjectLeftOf(left, morphism)
    }
  }

}